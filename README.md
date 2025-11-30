# Maguito de Batalla

**Maguito de Batalla** es un juego escrito en **Haskell** usando la librería **Gloss** para gráficos y manejo de eventos, en el que controlas al poderoso mago "Maguito" que se mueve por un mapa cuadrado mientras automáticamente dispara a los enemigos que se acercan. A medida que avanza el juego puede recolectar poderosos objetos que incrementan la potencia de sus hechizos de combate mientras busca sobrevivir el incesante oleaje de monstruos enemigos que buscan su muerte.

---

## Contenido

* `main.hs`: Código completo del juego.
* `Makefile`: Herramienta para compilación del juego
* Assets: No requiere externos más allá de la librería Gloss.
* Se implementa todo el juego usando **Monad State** para actualizar el estado de manera segura y funcional.

---

## Objetivo del juego

* Mover a Maguito usando las flechas del teclado.
* Evitar que los enemigos toquen a Maguito.
* Sobrevivir el mayor tiempo posible mientras el puntaje aumenta por cada enemigo eliminado.

---

## Controles

* **Flechas direccionales**: Mover al jugador.
* **En el menú**:

  * `1`: Iniciar partida.
  * `2`: Ver instrucciones.
  * `3`: Salir del juego.
* **Pantalla de instrucciones**: `b` Para volver al menú principal.
* **Game Over**: `r` para reiniciar.

---

## Pantallas

* **Menu**: Pantalla inicial con opciones.
* **Instrucciones**: Explicación de controles y objetivos.
* **Juego**: Pantalla principal con jugador, enemigos, obstáculos balas y objetos.
* **Game Over**: Se muestra al perder toda la vida del jugador.

---

## Mecánicas (parte técnica)

### Movimiento

* El jugador se mueve con las flechas. La posición se limita al mapa usando la función `clamp`.
* Los enemigos se mueven automáticamente hacia el jugador mediante `moveTowards`.

### Balas y disparo automático

* Cada `shootInterval` segundos, Maguito dispara automáticamente hacia el **enemigo más cercano**.
* Las balas tienen velocidad constante (`bulletSpeed`).

### Colisiones

* **Bala - Enemigo**: Si la distancia es menor que la suma de los radios (`bulletRadius + enemyRadius`), se elimina el enemigo y la bala.
* **Jugador - Enemigo**: El jugador pierde 10 HP por enemigo que lo toque. Si HP ≤ 0, termina la partida.

### Spawn progresivo de enemigos

* Los enemigos aparecen en posiciones aleatorias fuera del mapa.
* Con el tiempo, el intervalo de spawn disminuye para aumentar la dificultad.

---

## Uso de la Monad State

El juego hace un uso intensivo de **`State GameState`** para actualizar el estado de forma funcional:

* `movePlayer`: Actualiza la posición del jugador.
* `updateTimers`: Incrementa temporizadores (`elapsedTime`, `spawnTimer`, `shootTimer`).
* `spawnEnemiesProgressively`: Genera enemigos nuevos según el tiempo transcurrido.
* `moveEnemies`: Dirige todos los enemigos hacia el jugador.
* `autoShoot`: Dispara automáticamente hacia el enemigo más cercano.
* `moveBullets`: Actualiza la posición de todas las balas.
* `killCollision` y `deathCollision`: Detectan colisiones y actualizan HP, enemigos y puntaje.

El flujo general de actualización es:

```haskell
updateGame dt = do
  updateTimers dt
  movePlayer ...
  spawnEnemiesProgressively
  autoShoot
  moveBullets dt
  moveEnemies dt
  killCollision
  deathCollision
```

Luego se ejecuta con `execState` dentro de la función `update` exigida por Gloss:

```haskell
update :: Float -> GameState -> GameState
update dt gs@GameState{instScreen = ScreenGame} =
  execState (updateGame dt) gs
```

Esto asegura que **cada frame se modifique el estado de manera pura y secuencial**, manteniendo la lógica del juego separada de la representación.

---

## Visuales 

* **Jugador**: Maguito, el mago.
* **Enemigos**: Demonios rojos.
* **Balas**: Bolas de fuego.
* **Mapa**: cuadrado delimitando el área de juego, con temática de castillo gótico.
* **HUD**: barra de vida, puntaje y tiempo.

---

## Cómo ejecutar

1. Instalar Haskell y Gloss.

2. Compilar y correr (hacer uso de `make` en consola).

3. Disfrutar del juego.

---

## Notas extra

* Se utiliza `unsafePerformIO` solo para generar posiciones aleatorias de enemigos dentro de la función pura `randomSpawnPosition`.
* El juego sigue un **bucle de actualización y renderizado** constante a 60 FPS.

---

## Autores:
1. Jorge Cheuquemán.
2. Cristian Vera.

---

**¡Diviértete defendiendo a Maguito!**
