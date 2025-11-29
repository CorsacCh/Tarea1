{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

-- Gloss: Para dibujar gráficos y recibir input
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- State Monad: Para manejar el estado del juego progresivamente
import Control.Monad.State

-- Para calcular “enemigo más cercano”
import Data.List      (minimumBy)
import Data.Function  (on)

-- Para implementar aleatoriedad
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)

-- ============================================================
-- 1. DEFINICIÓN DE TIPOS DE DATOS (ESTADO DEL JUEGO)
-- ============================================================

-- Enemigo: por ahora solo tiene una posición (x,y)
data Enemy = Enemy
  { enemyPos :: (Float, Float)
  } deriving Show

-- Bala: tiene posición y velocidad (vx,vy)
data Bullet = Bullet
  { bulletPos :: (Float, Float)
  , bulletVel :: (Float, Float)
  } deriving Show

-- GameState: contiene TODO el estado del juego
-- Cada frame modificamos este estado usando la Monad State
data GameState = GameState
  { playerPos   :: (Float, Float)   -- posición del jugador
  , enemies     :: [Enemy]          -- lista de enemigos activos
  , elapsedTime :: Float            -- tiempo total desde que inició el juego
  , spawnTimer  :: Float            -- tiempo desde último spawn de enemigos
  , bullets     :: [Bullet]         -- todas las balas activas
  , shootTimer  :: Float            -- tiempo desde último disparo automático
  , playerDir   :: (Float, Float)   -- dirección a la que el jugador quiere ir
  } deriving Show

-- Estado inicial del juego
initialState :: GameState
initialState = GameState
  { playerPos   = (0, 0)
  , enemies     = []     -- comenzamos sin enemigos
  , elapsedTime = 0
  , spawnTimer  = 0
  , bullets     = []
  , shootTimer  = 0
  , playerDir = (0, 0)
  }

-- ============================================================
-- 2. PARÁMETROS GENERALES DEL JUEGO
-- ============================================================

halfMapSize :: Float
halfMapSize = 200  -- tamaño del mapa (cuadrado)

playerSpeed :: Float
playerSpeed = 10   -- rapidez de movimiento por tecla

playerRadius :: Float
playerRadius = 10  -- tamaño del jugador (triángulo)

enemyRadius :: Float
enemyRadius = 8    -- tamaño de cada enemigo (círculo)

bulletRadius :: Float
bulletRadius = 4   -- tamaño de cada bala

enemySpeed :: Float
enemySpeed = 40    -- velocidad con que los enemigos persiguen al jugador

bulletSpeed :: Float
bulletSpeed = 250  -- velocidad de las balas

shootInterval :: Float
shootInterval = 0.5 -- cada cuánto dispara el jugador automáticamente

-- ============================================================
-- 3. MOVIMIENTO DEL JUGADOR
-- ============================================================

-- movePlayer actualiza la posición del jugador
-- pero cuidando que no salga del mapa
movePlayer :: (Float, Float) -> State GameState ()
movePlayer (dx, dy) = do
  GameState{..} <- get      -- obtenemos el estado actual
  let (x, y) = playerPos
      newX = clamp (-halfMapSize + playerRadius)
                   ( halfMapSize - playerRadius ) (x + dx)
      newY = clamp (-halfMapSize + playerRadius)
                   ( halfMapSize - playerRadius ) (y + dy)

  -- Guardamos el nuevo estado
  put GameState
    { playerPos   = (newX, newY)
    , ..   -- deja el resto igual (enemigos, tiempo, balas, etc.)
    }

-- Limita un valor dentro de un rango [mn, mx]
clamp :: Float -> Float -> Float -> Float
clamp mn mx v = max mn (min mx v)

-- ============================================================
-- 4. ACTUALIZACIÓN DE TEMPORIZADORES
-- ============================================================

-- updateTimers suma dt (tiempo entre frames) a:
-- - elapsedTime
-- - spawnTimer
-- - shootTimer
updateTimers :: Float -> State GameState ()
updateTimers dt = do
  GameState{..} <- get
  put GameState
    { elapsedTime = elapsedTime + dt
    , spawnTimer  = spawnTimer  + dt
    , shootTimer  = shootTimer + dt
    , ..
    }

-- ============================================================
-- 5. SPAWN PROGRESIVO DE ENEMIGOS
-- ============================================================

-- A medida que pasa el tiempo, el spawn se hace más rápido
spawnEnemiesProgressively :: State GameState ()
spawnEnemiesProgressively = do
  GameState{..} <- get

  let maxTime      = 180      -- 3 minutos
      minInterval  = 0.4      -- intervalo más rápido
      maxInterval  = 3.0      -- intervalo más lento

      progress        = min 1 (elapsedTime / maxTime)
      currentInterval = maxInterval - progress * (maxInterval - minInterval)

  if spawnTimer < currentInterval
    then return () -- todavía no toca spawnear
    else do
      let newEnemy = Enemy (randomSpawnPosition progress)
      put GameState
        { enemies    = newEnemy : enemies
        , spawnTimer = 0
        , ..
        }

-- Aparición de enemigos en posiciones aleatorias
randomSpawnPosition :: Float -> (Float, Float)
randomSpawnPosition prog = unsafePerformIO (do
  let baseDist = 220
      extraDist = 200 * prog
      d = baseDist + extraDist

  side <- randomRIO (1 :: Int, 4)

  let maxOffset = halfMapSize + 100 * prog
  offset <- randomRIO (-maxOffset, maxOffset)

  return $ case side of
    1 -> (-d, offset)
    2 -> ( d, offset)
    3 -> (offset,  d)
    _ -> (offset, -d)
  )

-- ============================================================
-- 6. MOVIMIENTO DE ENEMIGOS
-- ============================================================

-- Todos los enemigos se mueven hacia el jugador
moveEnemies :: Float -> State GameState ()
moveEnemies dt = do
  GameState{..} <- get

  let moved =
        [ Enemy ( moveTowards enemyPos playerPos enemySpeed dt )
        | Enemy{enemyPos} <- enemies
        ]

  put GameState
    { enemies = moved
    , ..
    }

-- Mueve un punto A hacia B
moveTowards :: (Float,Float) -> (Float,Float) -> Float -> Float -> (Float,Float)
moveTowards (ex,ey) (px,py) speed dt =
  let dx = px - ex
      dy = py - ey
      dist = sqrt (dx*dx + dy*dy)
      (ux,uy) = if dist == 0 then (0,0) else (dx/dist, dy/dist)
  in ( ex + ux * speed * dt
     , ey + uy * speed * dt
     )

-- ============================================================
-- 7. DISPARO AUTOMÁTICO HACIA EL ENEMIGO MÁS CERCANO
-- ============================================================

autoShoot :: State GameState ()
autoShoot = do
  GameState{..} <- get

  -- si todavía no ha pasado el intervalo o no hay enemigos, no disparamos
  if shootTimer < shootInterval || null enemies
    then return ()
    else case closestEnemy playerPos enemies of
      Nothing -> return ()
      Just (Enemy epos) -> do
        let bullet = createBulletTowards playerPos epos bulletSpeed
        put GameState
          { bullets    = bullet : bullets
          , shootTimer = 0
          , ..
          }

-- Encuentra el enemigo más cercano calculando distancia^2
closestEnemy :: (Float,Float) -> [Enemy] -> Maybe Enemy
closestEnemy _ [] = Nothing
closestEnemy (px,py) es =
  let distSq (Enemy (ex,ey)) =
        let dx = px - ex
            dy = py - ey
        in dx*dx + dy*dy
  in Just (minimumBy (compare `on` distSq) es)

-- Crea una bala desde el jugador hacia un enemigo
createBulletTowards :: (Float,Float) -> (Float,Float) -> Float -> Bullet
createBulletTowards (px,py) (ex,ey) speed =
  let dx = ex - px
      dy = ey - py
      dist = sqrt (dx*dx + dy*dy)
      (ux,uy) = if dist == 0 then (0,1) else (dx/dist, dy/dist)
  in Bullet
       { bulletPos = (px,py)
       , bulletVel = (ux*speed, uy*speed)
       }

-- colisión entre balas y enemigos
killCollision :: State GameState ()
killCollision = do
  GameState{..} <- get

  let hitRange = enemyRadius + bulletRadius

      -- enemigos que NO fueron golpeados
      enemiesSurvivors =
        [ e
        | e@(Enemy epos) <- enemies
        , all (\(Bullet bpos _) -> dist epos bpos >= hitRange) bullets
        ]

      -- balas que NO golpearon a ningún enemigo
      bulletsSurvivors =
        [ b
        | b@(Bullet bpos _) <- bullets
        , all (\(Enemy epos) -> dist epos bpos >= hitRange) enemies
        ]

  put GameState
    { enemies = enemiesSurvivors
    , bullets = bulletsSurvivors
    , ..
    }

-- ============================================================
-- 8. MOVIMIENTO DE BALAS
-- ============================================================

moveBullets :: Float -> State GameState ()
moveBullets dt = do
  GameState{..} <- get

  let bullets' =
        [ b { bulletPos = (x', y') }
        | b@Bullet{bulletPos = (x,y), bulletVel = (vx,vy)} <- bullets
        , let x' = x + vx*dt
              y' = y + vy*dt
        -- descartamos balas que salgan demasiado lejos
        , abs x' <= halfMapSize * 2
        , abs y' <= halfMapSize * 2
        ]

  put GameState
    { bullets = bullets'
    , ..
    }

-- ============================================================
-- 9. FUNCIÓN UPDATE GENERAL
-- ============================================================

-- updateGame usa State para modificar el estado paso a paso
updateGame :: Float -> State GameState ()
updateGame dt = do
  updateTimers dt

  GameState{playerDir = (dx, dy)} <- get
  movePlayer (dx * playerSpeed, dy * playerSpeed)

  spawnEnemiesProgressively
  autoShoot
  moveBullets dt
  moveEnemies dt
  killCollision

-- función update exigida por Gloss
update :: Float -> GameState -> GameState
update dt gs = execState (updateGame dt) gs

-- ============================================================
-- 10. DIBUJAR PERSONAJE, ENEMIGOS Y BALAS
-- ============================================================

render :: GameState -> Picture
render GameState{..} =
  Pictures $
    [ translate 0 0 (color white mapPicture)                  -- mapa
    , uncurry translate playerPos (color green playerPicture) -- jugador
    ]
    ++ map drawEnemy enemies                                  -- enemigos
    ++ map drawBullet bullets                                 -- balas

-- Dibuja un enemigo como círculo rojo
drawEnemy :: Enemy -> Picture
drawEnemy (Enemy (x,y)) =
  translate x y $
    color red $
      circleSolid enemyRadius

-- Dibuja una bala como círculo amarillo
drawBullet :: Bullet -> Picture
drawBullet (Bullet (x,y) _) =
  translate x y $
    color yellow $
      circleSolid bulletRadius

-- Mapa cuadrado
mapPicture :: Picture
mapPicture =
  lineLoop
    [ (-halfMapSize, -halfMapSize)
    , (-halfMapSize,  halfMapSize)
    , ( halfMapSize,  halfMapSize)
    , ( halfMapSize, -halfMapSize)
    ]

-- Triángulo que representa al jugador
playerPicture :: Picture
playerPicture =
  polygon
    [ ( 0,            playerRadius)
    , (-playerRadius, -playerRadius)
    , ( playerRadius, -playerRadius)
    ]

-- ============================================================
-- 11. EVENTOS DE TECLADO (para movimiento del jugador)
-- ============================================================

handleEvent :: Event -> GameState -> GameState
handleEvent event gs@GameState{playerDir = (dx, dy)} =
  case event of
    EventKey (SpecialKey KeyUp) Down _ _    -> gs {playerDir = (dx, 1)}   --arriba
    EventKey (SpecialKey KeyUp) Up   _ _    -> gs {playerDir = (dx, 0)}

    EventKey (SpecialKey KeyDown) Down _ _  -> gs {playerDir = (dx, -1)}  --abajo
    EventKey (SpecialKey KeyDown) Up   _ _  -> gs {playerDir = (dx, 0)}

    EventKey (SpecialKey KeyRight) Down _ _ -> gs {playerDir = (1, dy)}   --derecha
    EventKey (SpecialKey KeyRight) Up   _ _ -> gs {playerDir = (0, dy)}

    EventKey (SpecialKey KeyLeft) Down _ _  -> gs {playerDir = (-1, dy)}  --izquierda
    EventKey (SpecialKey KeyLeft) Up   _ _  -> gs {playerDir = (0, dy)}

    _ -> gs

-- ============================================================
-- 12. MISCELÁNEA
-- ============================================================

dist :: (Float,Float) -> (Float,Float) -> Float
dist (x1,y1) (x2,y2) =
  sqrt ((x1 - x2)^2 + (y1 - y2)^2)

-- ============================================================
-- 13. MAIN
-- ============================================================

main :: IO ()
main = do
  let window = InWindow "Bullet Heaven Haskell" (800, 800) (100, 100)
      backgroundColor = black
      fps = 60
  play window backgroundColor fps initialState render handleEvent update

