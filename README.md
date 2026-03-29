# Lógica Computacional 2026-2

## Práctica 3

Para trabajar sobre esta base, tienen que hacer un fork de este repositorio y trabajar sobre él.

Únicamente modifiquen el archivo Practica03.hs que se encuentra en el directorio src. Si quieren modificar las pruebas o agregar más, pueden preguntarme con confianza y les explico como modificarlas.

Deben tener instalado el compilador de Haskell para poder probar su práctica. Para ello deben colocarse en el directorio src y ejecutar el comando `ghci Practica03.hs`.

Si quieren probar su práctica haciendo uso de las pruebas unitarias que les estoy pasando, tienen que ejecutar los siguientes comandos desde el directorio donde se encuentra este ReadMe:
```
cabal build
cabal test
```

El primero es para compilar y el segundo es para ejecutar las pruebas unitarias.

Si no les llegan a funcionar, es posible que el problema es que tengan una versión diferente de cabal y de ghc. Si ese es el caso, pueden ejecutar el comando `ghc-pkg list base` para reemplazar la versión base que viene en el archivo .cabal en las líneas 70 y 102.

En este caso particular es posible que también necesiten ejecutar `ghc-pkg list deepseq` para igualmente reemplazar la versión de ese paquete que viene en el archivo .cabal en la línea 104.

## Integrantes

En esta sección deben eliminar esta línea de texto, borrar la leyenda "Integrante n" y escribir su nombre empezando por apellidos y su número de cuenta.

+ Sánchez Pastrana Rafael
    - No. de Cuenta: 322285930
+ Ayala Montiel María Fernanda
    - No. de Cuenta: 321309347
+ Carriche Arriaga Dante Raziel
    - No. de Cuenta: 322037461

## Comentarios

En cuanto a las pruebas, sucede que en los tests de forma normal conjuntiva con la formula Cr y forma normal conjuntiva con truco no dan las mismas expresiones, porque están en orden distinto. Por ejemplo, en la forman normal conjuntiva con truco la función espera es (((p V s) V q) Y ((r V s) V q)), pero nosotros regresamos ((q V (s V p)) Y (q V (s V r))), que son la misma fórmula pero en distinto orden.

Por otra parte, en la Fórmula con truco en el test de saturación, se logra resolver el problema regresando True, lo cual no estamos seguros de si es un error, pero se dice que no se puede resolver en el test, pero nuestra implementación si llega a una conclusión, que de hecho coincide con lo que dice luego de la parte que dice deepseq return True. Pero puede que sea un error.
