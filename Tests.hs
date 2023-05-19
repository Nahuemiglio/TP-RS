module Tests where

import Test.HUnit
import Solucion

main = runTestTT todosLosTest

todosLosTest = test [testsuite1, testsuite2, testsuite3, testsuite4, testsuite5, testsuite6, testsuite7, testsuite8, testsuite9, testsuite10]

testsuite1 = test [
    "Caso 1: Red social vacía (sin usuarios)" ~: (nombresDeUsuarios redA) ~?= [],
    "Caso 2: Red social con un solo usuario" ~: (nombresDeUsuarios redB) ~?= ["Juan"],
    "Caso 3: Red social con más de un usuario y un nombre repetido" ~: (nombresDeUsuarios redC) ~?= ["Juan", "Natalia", "Pedro", "Mariela", "Matias", "Carlos"],
    "Caso 4: Red social con más de un usuario y ningún nombre repetido" ~: (nombresDeUsuarios redD) ~?= ["Juan","Natalia", "Pedro", "Mariela", "Carlos", "Ernesto"]
 ]

testsuite2 = test [
    "Caso 1: Usuario A es amigo del Usuario B" ~: (amigosDe redE usuario1) ~?= [usuario4],
    "Caso 2: Usuario B es amigo del Usuario A" ~: (amigosDe redE usuario4) ~?= [usuario1],
    "Caso 3: El usuario no tiene amigos" ~: (amigosDe redE usuario6) ~?= [],
    "Caso 4: El usuario tiene más de un amigo" ~: (amigosDe redD usuario2) ~?= [usuario3, usuario4, usuario7]
 ]

testsuite3 = test [
    "Caso 1: El usuario no tiene amigos" ~: (cantidadDeAmigos redE usuario6) ~?= 0,
    "Caso 2: El usuario tiene un solo amigo" ~: (cantidadDeAmigos redC usuario7) ~?= 1,
    "Caso 3: El usuario tiene más de un amigo" ~: (cantidadDeAmigos redD usuario2) ~?= 3
 ]

testsuite4 = test [
    "Caso 1: Red con una sola relación" ~: (usuarioConMasAmigos redE) `pertenece` [usuario1, usuario4] ~?= True,
    "Caso 2: Red con varios usuarios, de los cuales dos tienen la misma (y mayor) cantidad de amistades" ~: (usuarioConMasAmigos redD) `pertenece` [usuario2, usuario3] ~?= True,
    "Caso 3: Red con varios usuarios y relaciones, de los cuales uno tiene más amistades que los demás" ~: (usuarioConMasAmigos redC) ~?= usuario1,
    "Caso 4: Red con un solo usuario" ~: (usuarioConMasAmigos redB) ~?= usuario1
 ]

testsuite5 = test [
    "Caso 1: Todos los usuarios tienen menos de 10 amigos" ~: (estaRobertoCarlos redD) ~?= False,
    "Caso 2: Todos los usuarios tienen 10 amigos o menos" ~: (estaRobertoCarlos redF) ~?= False,
    "Caso 3: Hay un usuario con más de 10 amigos" ~: (estaRobertoCarlos redG) ~?= True
 ]

testsuite6 = test  [
    "Caso 1: El usuario no tiene publicaciones" ~: (publicacionesDe redC usuario2) ~?= [],
    "Caso 2: El usuario tiene publicaciones" ~: (publicacionesDe redC usuario3) ~?= [publicacion3_1, publicacion3_2],
    "Caso 3: No hay publicaciones en la red" ~: (publicacionesDe redD usuario7) ~?= []
 ]

testsuite7 = test  [
    "Caso 1: Al usuario no le gustó ninguna publicación" ~: (publicacionesQueLeGustanA redC usuario6) ~?= [],
    "Caso 2: Al usuario le gustó una o más publicaciones" ~: (publicacionesQueLeGustanA redC usuario2) ~?= [publicacion3_1, publicacion3_2, publicacion1_2],
    "Caso 3: No hay publicaciones en la red" ~: (publicacionesQueLeGustanA redD usuario4) ~?= []
 ]

testsuite8 = test  [
    "Caso 1: No hay publicaciones con algún 'me gusta' en común" ~: (lesGustanLasMismasPublicaciones redC usuario3 usuario4) ~?= False,
    "Caso 2: No todas las publicaciones tienen 'me gusta' en común" ~: (lesGustanLasMismasPublicaciones redG usuario2 usuario4) ~?= False,
    "Caso 3: A ningún usuario le gustó al menos una publicación" ~: (lesGustanLasMismasPublicaciones redG usuario10 usuario11) ~?= True,
    "Caso 4: A los dos usuarios les gustaron exactamente las mismas publicaciones" ~: (lesGustanLasMismasPublicaciones redF usuario1 usuario5) ~?= True
 ]

testsuite9 = test [
    "Caso 1: El usuario no tiene publicaciones" ~: (tieneUnSeguidorFiel redD usuario4) ~?= False,
    "Caso 2: El usuario tiene publicaciones sin 'me gusta'" ~: (tieneUnSeguidorFiel redF usuario6) ~?= False,
    "Caso 3: El usuario no tiene 'me gusta' del mismo usuario en todas sus publicaciones" ~: (tieneUnSeguidorFiel redG usuario1) ~?= False,
    "Caso 4: El usuario tiene 'me gusta' del mismo usuario en todas sus publicaciones" ~: (tieneUnSeguidorFiel redG usuario2) ~?= True
 ]

testsuite10 = test [
    "Caso 1: Hay una única relación en la red" ~: (existeSecuenciaDeAmigos redE usuario1 usuario4) ~?= True,
    "Caso 2: Hay más de una relación en la red, pero el Usuario A y el Usuario B no están conectados de manera indirecta" ~: (existeSecuenciaDeAmigos redH usuario1 usuario4) ~?= False,
    "Caso 3: Hay más de una relación en la red y ambos usuarios están conectados de manera indirecta" ~: (existeSecuenciaDeAmigos redH usuario4 usuario12) ~?= True,
    "Caso 4: Hay al menos dos usuarios en la red pero no existe ninguna relación" ~: (existeSecuenciaDeAmigos redI usuario6 usuario10) ~?= False,
    "Caso 5: Cadena de amigos entre un mismo usuario si el usuario tiene una relación" ~: (existeSecuenciaDeAmigos redJ usuario1 usuario1) ~?= True
 ]





-- VARIABLES DE TEST --

-- USUARIOS:
usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Matias")
usuario7 = (7, "Carlos")
usuario8 = (8, "Pepe")
usuario9 = (9, "Marcos")
usuario10 = (10, "Ernesto")
usuario11 = (11, "Agustina")
usuario12 = (12, "Roberto Carlos")

-- RELACIONES:

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario1, usuario4)
relacion1_7 = (usuario1, usuario7)

relacion2_3 = (usuario2, usuario3)
relacion2_4 = (usuario2, usuario4)
relacion2_7 = (usuario2, usuario7)

relacion3_4 = (usuario3, usuario4)
relacion3_6 = (usuario3, usuario6)

relacion12_1 = (usuario12, usuario1)
relacion12_2 = (usuario12, usuario2)
relacion12_3 = (usuario12, usuario3)
relacion12_4 = (usuario12, usuario4)
relacion12_5 = (usuario12, usuario5)
relacion12_6 = (usuario12, usuario6)
relacion12_7 = (usuario12, usuario7)
relacion12_8 = (usuario12, usuario8)
relacion12_9 = (usuario12, usuario9)
relacion12_10 = (usuario12, usuario10)
relacion12_11 = (usuario12, usuario11)

-- PUBLICACIONES:

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario3])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario2, usuario4])

publicacion2_1 = (usuario2, "Este es mi primer post", [usuario4, usuario5])
publicacion2_2 = (usuario2, "Este es mi segundo post", [usuario4])
publicacion2_3 = (usuario2, "Este es mi tercer post", [usuario3, usuario4])

publicacion3_1 = (usuario3, "Este es mi primer post", [usuario2])
publicacion3_2 = (usuario3, "Este es mi segundo post", [usuario2, usuario4])
publicacion3_3 = (usuario3, "Este es mi tercer post", [usuario1, usuario5])

publicacion4_1 = (usuario4, "Este es mi primer post", [usuario1, usuario5])
publicacion4_2 = (usuario4, "Este es mi segundo post", [usuario1, usuario5, usuario2, usuario3])

publicacion5_1 = (usuario5, "Este es mi primer post", [usuario1])
publicacion5_2 = (usuario5, "Este es mi segundo post", [usuario1, usuario3])
publicacion5_3 = (usuario5, "Este es mi tercer post", [usuario9])

publicacion6_1 = (usuario6, "Este es mi primer post", [])
publicacion6_2 = (usuario6, "Este es mi segundo post", [usuario2])


-- REDES --

usuariosA = []
relacionesA = []
publicacionesA = []
redA = (usuariosA, relacionesA, publicacionesA)


usuariosB = [usuario1]
relacionesB = []
publicacionesB = [publicacion1_1]
redB = (usuariosB, relacionesB, publicacionesB)


usuariosC = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7]
relacionesC = [relacion1_2, relacion3_6, relacion1_3, relacion1_7]
publicacionesC = [publicacion3_1, publicacion3_2, publicacion1_1, publicacion1_2]
redC = (usuariosC, relacionesC, publicacionesC)


usuariosD = [usuario1, usuario2, usuario3, usuario4, usuario7, usuario10]
relacionesD = [relacion2_3, relacion2_4, relacion2_7, relacion1_3, relacion3_4]
publicacionesD = []
redD = (usuariosD, relacionesD, publicacionesD)


usuariosE = [usuario1, usuario4, usuario6, usuario10]
relacionesE = [relacion1_4]
publicacionesE = []
redE = (usuariosE, relacionesE, publicacionesE)


usuariosF = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relacionesF = [relacion12_1, relacion12_2, relacion12_3, relacion12_4, relacion12_5, relacion12_6, relacion12_7, relacion12_8, relacion12_9, relacion12_10]
publicacionesF = [publicacion1_1, publicacion3_3, publicacion4_1, publicacion6_1]
redF = (usuariosF, relacionesF, publicacionesF)


usuariosG = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relacionesG = [relacion12_1, relacion12_2, relacion12_3, relacion12_4, relacion12_5, relacion12_6, relacion12_7, relacion12_8, relacion12_9, relacion12_10, relacion12_11]
publicacionesG = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion2_3, publicacion3_1, publicacion4_2, publicacion6_1]
redG = (usuariosG, relacionesG, publicacionesG)


usuariosH = [usuario1, usuario2, usuario3, usuario4, usuario6, usuario7, usuario12]
relacionesH = [relacion1_2, relacion1_7, relacion3_4, relacion3_6, relacion12_6]
publicacionesH = []
redH = (usuariosH, relacionesH, publicacionesH)


usuariosI = [usuario1, usuario2]
relacionesI = []
publicacionesI = []
redI = (usuariosI, relacionesI, publicacionesI)


usuariosJ = [usuario1, usuario2]
relacionesJ = [relacion1_2]
publicacionesJ = []
redJ = (usuariosJ, relacionesJ, publicacionesJ)