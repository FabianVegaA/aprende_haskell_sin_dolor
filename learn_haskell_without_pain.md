# Aprende Haskell sin dolor

Este artículo es una introducción a Haskell para todo aquel que proviene de un lenguaje de alto nivel como Python, Ruby, Java, etc.

## Índice

- [Aprende Haskell sin dolor](#aprende-haskell-sin-dolor)
  - [Índice](#índice)
  - [Instalación](#instalación)
  - [Primeros pasos](#primeros-pasos)
    - [Crea tu primera función](#crea-tu-primera-función)
      - [Guard](#guard)
      - [If](#if)
      - [Case](#case)
    - [Cómo llamar a una función](#cómo-llamar-a-una-función)
  - [Más ejemplos de Funciones](#más-ejemplos-de-funciones)
  - [Todo es una función](#todo-es-una-función)
  - [Manejo de Inputs y Outputs](#manejo-de-inputs-y-outputs)
  - [Adiós iteradores](#adiós-iteradores)
  - [Where y Let](#where-y-let)
  - [List Comprehension](#list-comprehension)
  - [Lazyness](#lazyness)
  - [Referencias](#referencias)

Siempre que hablo con otros desarrolladores de software sobre este lenguaje, me dicen "Intenté aprenderlo, pero su sintaxis es... un poco peculiar", y me parece que tienen razón.

La sintaxis a la que la mayoría de los programadores están acostumbrados, pertenece a algún lenguaje con paradigma imperativo. Estos apuntan principalmente a procesos y a iterar sobre listas u otra estructura de datos. De este modo, todo lo que se salga de esa regla parecerá raro, pero eso no debiese ser en un obstáculo para aprender cosas nuevas y salir de la zona de confort.

Aprender un nuevo lenguaje de programación, librería, framework o incluso un nuevo paradigma, es una tarea difícil, pero no imposible, que te da la oportunidad de mejorar y ampliar tu stack de conocimientos.

Así que comencemos. Intentaré enseñarte la superficie de este lenguaje de programación y mostrarte un poco de lo que es la programación funcional.

## Instalación

Si deseas comenzar a probar este lenguaje debes instalar su compilador llamado `GHC`, para ello te recomiendo seguir los pasos que aparecen [aquí](https://www.fpcomplete.com/haskell/get-started/). También puedes instalar un manejador de paquetes como **cabal** o **stack**, estos te harán mucho más fácil construir proyectos con Haskell, tal como son **poetry** y **pipenv** en Python.

## Primeros pasos

Antes de comenzar, lo más importante, tu primer Hello World con Haskell. Para ello abre una terminal y escribe `ghci` para abrir su intérprete y luego `putStrLn "Hello World"`. ¡Felicidades! Ya puedes decir que eres un desarrollador Haskell.

```console
$ ghci
Prelude> putStrLn "Hello World"
```

Fuera de bromas, Haskell es un lenguaje de paradigma funcional puro, lo que implica que mantiene la transparencia referencial y no posee side effects. Además, es un lenguaje con tipificado fuerte y estático.

**¿Y en español qué significa?**

En otras palabras, en este lenguaje no tenemos variables como tal, solo valores que se pueden asignar, pero no alterarse. Además si llamamos a una función con ciertos argumentos y nos devuelve un valor, podemos esperar que esa función siempre responda igual con los mismos argumentos. Lo que nos entrega determinismo.

En cuanto al tipificado, significa que si se asigna un valor a una variable con tipo `Int`, ese valor no puede ser cambiado a otro valor de tipo `String` o `Float`. Y ni intentes sumar un `Int` con un `String` como en JavaScript o Scala.

Tal como se puede intuir del nombre del paradigma en Haskell el protagonismo lo poseen las funciones y las diferentes maneras como se juntan para programar algoritmos.

### Crea tu primera función

```haskell
doubleMe :: Int -> Int -- Firma de la función
doubleMe x = x + x     -- Definición de la función
```

Equivalente a:

```python
def double_me(x: int) -> int:
    return x + x
```

A diferencia de la mayoría de lenguajes, aquí no es necesario utilizar el `return` para retornar un valor, en Haskell todo lo que esté luego del `=` es lo que se retorna.

Esta función se llama `doubleMe` y las flechas de arriba significan que esta función recibe un `Int` y devuelve un `Int`. Siempre que quieras leer la firma de una función en Haskell debes tomar en cuenta que los primeros $n-1$ tipos son los argumentos de la función y el último lo que retorna.

Este lenguaje posee algo muy interesante, llamado Pattern Matching, que nos permite definir nuestras funciones en diferentes situaciones.

Intentemos con otra función:

```haskell
sayHello :: String -> String
sayHello "Mundo" = "¡¡Hola, Mundo!!"
sayHello "World" = "Hello, World!!"
sayHello name = "Hola, " ++ name
```

La función `sayHello` recibe un `String` y devuelve un `String`.

Además, se tienen 3 casos, el primero si se recibe `"Mundo"`, segundo `"World"` y tercero se guarda la entrada en una variable `name` y se concatena con `"Hola, "`.

Como dato curioso un `String` es una lista de caracteres, dicho de otro modo es `[Char]`, `String` puedes pensarlo como un sinónimo para facilitar la lectura. Por otro lado, el operador `++` se usa para concatenar dos listas.

Además del Pattern Mathing podemos utilizar **guards**, **ifs** y **case** como estructuras de control de flujo.

Aquí va un breve ejemplo de los 3 casos:

#### Guard

Esta es otra estructura que nos permite manejar el flujo de nuestros programas, es semejante a un switch de otros lenguajes como C.

Su sintaxis es:

```haskell
<name_function> :: a -> b
<name_function> x
    | <condition> = "This is the first result"
    | <condition> = "This is the second result"
    | otherwise   = "This is the last result"
```

Una implementación podría ser:

```haskell
bmiTell :: Float -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight!"
    | bmi <= 25.0 = "You're supposedly normal!"
    | bmi <= 30.0 = "You should lose some weight"
    | otherwise   = "That is worrying!"
```

#### If

Tal como en casi todos los lenguajes tenemos los `if` la única diferencia es que no tenemos `else if` o `elif`.

```haskell
even :: Int -> Bool
even n = if mod n 2 == 0
    then True
    else False
```

#### Case

Para los casos en que necesitamos más condiciones también podemos usar los `case`:

```haskell
describeList :: [a] -> String
describeList xs =
  "The list is " ++ case xs of
    [] -> "empty."
    [x] -> "a singleton list."
    xs -> "a longer list."
```

Este último caso es especial, ya que empleamos `[a]` en la firma de la función, lo que significa que la función recibe una lista de lo que sea y devuelve un `String`. Esto lo veremos más adelante con detalle.

Ahora bien, ¿qué significa `[]`, `[x]` y `xs`? El primero pregunta si la lista está vacía, el segundo si posee solo un elemento y el tercero por cualquier número de elementos.

Cabe destacar que en el caso de `[x]` y `xs` se está almacenando el valor en una variable, por ejemplo, si la lista es `[1]`, `x` es `1`, porque `[1]` calza con el patrón `[x]`, pero si la lista es `[1,2,3]`, `xs` es `[1,2,3]`. Esto también es un tipo de Pattern Matching.

### Cómo llamar a una función

Tal vez ya lo notaste, pero Haskell a diferencia de otros lenguajes, no utiliza este tipo de notación `sayHello(name)` para llamar una función como en Python, sino `sayHello name` y con más argumentos es igual, nada más se debe colocar más espacios. Lo que te ahorra un montón de paréntesis 😅.

## Más ejemplos de Funciones

¿Y si quiero crear una función que reciba más argumentos? Para ello solo debes usar más flechas (->) como por ejemplo:

```haskell
elem :: Eq a => a -> [a] -> Bool
```

`elem` es una función que recibe un elemento y una lista y devuelve un `Bool`. `Eq a` es una manera de decir que el tipo de `a` debe ser comparable, también podemos usar `Ord a` para decir que el tipo debe ser ordenable y como estos existen muchos más.

Puedes ver más ejemplos de esto aquí:
![Typeclasses](https://cs.lmu.edu/~ray/images/haskell-typeclasses.png)

`Eq`, `Ord`, `Num`, `Enum`, entre otros, son ejemplos de Typeclasses, lo que nos permite condicionar una función para que funcione con ciertos tipos de datos que posean determinadas características.

## Todo es una función

En este lenguaje hasta los operadores son una función, es decir, suma `(+)`, concatenar `(++)`, resta `(-)`, multiplicación `(*)`, potencia `(^)` y muchos más, son funciones e incluso puedes crear tus propios operadores.

```haskell
(-+) :: Int -> Int -> [Int]
(-+) a b = [a - b, a + b]
```

Por ejemplo en el caso de arriba acabamos de generar un operador que nos da el valor de la resta y la suma de 2 `Int`.

```haskell
5 -+ 4 -- return: [1, 9]
```

## Manejo de Inputs y Outputs

Seguro ya debes estar sobrepasado de información, pero ahora vamos a ver la forma de recibir inputs y devolver outputs, pues es algo clave para, por ejemplo, hacer un CLI (Command Line Interface).

Una de las consecuencias de la programación funcional pura es que los inputs y outpus, deben mantenerse lejos de la pureza de las funciones 😇, por ello se tiene la `do notation` esta es una _syntax sugar_ que nos permite hacer una función que reciba inputs y devuelva outputs.

Para construir la función `main` en Haskell, debemos usar la notación `do`:

```haskell
main :: IO ()
main = do
    putStrLn "What's your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name)
```

Si estás pensando en que nunca más quieres ver un lenguaje de este paradigma, primero respira un poco, bebe agua y después haz una pregunta: ¿Qué es un `IO`?

Y yo te responderé `IO` es un Monad 🤯. Tranquilo... Es tan complejo como parece, pero es tema para otro día. Así que sigamos con lo siguiente que debió hacerte levantar una ceja, tal que así 🤨. ¿Qué es esa flecha `<-` en el `do`?

Eso significa que el resultado de getLine que es un `IO String` por ahora imagina a `IO` como una caja que contiene un `String`, y eso es lo que se guarda en `name`.

## Adiós iteradores

Algo que suele ser muy traumático en este lenguaje y en la programación funcional en general para los programadores es que aquí no existen estructuras como los `for`, `while`, ni `do while`, lo que suele confundir. Esto no significa que no haya otra manera de recorrer una lista, solo nos lleva usar otras formas de hacerlo.

Esto da el paso para hablar de un tema un tanto polémico, la **recursión**, en este lenguaje es un tema fundamental, pues si queremos recorrer una lista probablemente tendremos que recurrir a esto.

Por ejemplo, si quieres recorrer una lista y modificar sus elementos puedes usar

```haskell
iterateList :: (a -> b) -> [a] -> [b]
iterateList _ [] = []
iterateList f (x:xs) = f x : iterateList f xs
```

Vayamos por partes:

1. `(a -> b)` en la firma significa que recibe una función que a su vez recibe un argumento de tipo `a` y retorna un tipo `b`.
2. `_ []` quiere decir que ignoramos el primer argumento, es decir, la función y `[]` nos dice que recibe una lista vacía. Todo esto puedes leerlo como "si iterateList recibe una lista vacía sin importar que función sea, iterateList retornara una lista vacía".
3. `(x:xs)` es una manera de aprovechar el pattern matching guardando el primer valor de la lista en `x` y lo demás en `xs`, es decir, para `[1,2,3,4]`, `x = 1` y `xs = [2,3,4]`.

¡Sorpresa! Acabamos de descubrir la función `map` que debes haberlo visto en otros lenguajes. La función `iterateList` es lo mismo que `map` de Haskell.

```haskell
map odd [1,2,3,4] -- return: [True, False, True, False]
```

Tal como esto, existe `filter`, `foldl`, `foldl1` y muchos más, pero concentrémonos en los 2 últimas funciones.

```haskell
foldl :: (b -> a -> b) -> b -> t a -> b
```

Esta función es equivalente a `reduce` de Python y para entender su firma desglocemoslo por partes:

1. `(b -> a -> b)`: Esto es una función que recibe un tipo `b`, un `a` y retorna un `b`.
2. `b`: Para esta función este argumento es un valor inicial.
3. `t a`: Esto significa que `a` está en un Monad (en una "caja"), para lo que lo necesitamos nuestra caja es una lista. Sí, las listas también son Monads en Haskell.
4. `b`: Esto es el valor final de la función.

```haskell
foldl (+) 0 [1,2,3] -- return: 6
```

Como tal vez lo intuiste `foldl` lo que hace es `(((0 + 1) + 2) + 3)`.

Para `foldl1` es lo mismo solo que para el valor inicial se toma al primer valor de la lista. Es decir:

```haskell
foldl1 :: (b -> a -> b) -> t a -> b
```

```haskell
foldl1 (+) [1,2,3] -- return: 6
```

Y su procedimiento es `((1 + 2) + 3)`.

## Where y Let

Estas son palabras claves que nos permiten definir variables o funciones dentro de otra función, por ejemplo, si recordamos la función `bmiTell` podemos reescribirlo de este modo:

```haskell
bmiTell :: Float -> String
bmiTell weight height
    | bmi <= 18.5 = "You're underweight!"
    | bmi <= 25.0 = "You're supposedly normal!"
    | bmi <= 30.0 = "You should lose some weight"
    | otherwise   = "That is worrying!"
    where bmi = weight / height ^ 2
```

Y de igual manera podemos hacer con `let`, por ejemplo, si quisiéramos tener la función `volumeCylinder`

```haskell
volumeCyllinder :: Float -> Float -> String
volumeCyllinder height diameter =
  let radius = diameter / 2
      base = pi * (radius ^ 2)
   in "The volume of the cyllinder is " ++ show (base * height)
```

## List Comprehension

Si has usado Python seguro estas familiarizado con esta manera de definir listas, esto nos permite de manera sencilla generar nuevas listas. También de esta manera es posible construir nuestra propia función `filter` como viene a continuación.

**Haskell Version**

```haskell
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter cond values = [value | value <- values, cond value]
```

```haskell
myFilter odd [1..10] -- result: [1, 3, 5, 7, 9]
```

**Python Version**

```python
from typing import TypeVar

T = TypeVar("T")

def myFilter(cond: callable[[T],bool], values: list[T]):
    return [value for value in values if cond(value)]
```

```python
miFilter(lambda x: x % 2 != 0, range(10))) # result: [1, 3, 5, 7, 9]
```

## Lazyness

Cuando se trata de evaluar cosas Haskell es un lenguaje extremadamente perezoso, lo que conlleva que solo lo que es estrictamente necesario se evalúa. Esto tiene muchas ventajas, por ejemplo podemos crear listas infinitas y no tener problemas de memoria.

```haskell
[0 ..]    -- return: [1,2,3,4,5,6,7,8,...]
[2, 4 ..] -- return: [2,4,6,8,10,12,...]
[5, 8 ..] -- return: [5,8,11,14,17,... ]
```

También es posible construir nuevas funciones a partir de otras, por ejemplo:

```haskell
sumOne = (+1)
sumOne 2           -- return: 3

map sumOne [1,2,3] -- return: [2,3,4]
```

---

Espero que te haya gustado este tutorial y que haya sido de utilidad para tu llegada al lado oscuro, la programación funcional.

Haskell es un lenguaje que puede ser intimidante, pero una vez que entiendes sus bases, comprendes todo lo que puedes hacer con el y la facilidad con que puedes generar proyectos.

Por último, quiero remendarte estas herramientas, que te permitirán encontrar funciones, paquetes y más.

- [hoogle](https://hoogle.haskell.org)
- [haskellspot](https://www.haskellspot.com)
- [zvon](http://zvon.org/other/haskell/Outputglobal/index.html)

---

## Referencias

- [Haskell Beginners 2022](https://slides.com/haskellbeginners2022)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com)
