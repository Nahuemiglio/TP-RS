-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

--------------------------------------------------- Funciones basicas

-- Devuelve el conjunto de usuarios.
usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

-- Devuelve el conjunto de relaciones.
relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

-- Devuelve el conjunto de publicaciones.
publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

-- Devuelve el n ́umero de identificaci ́on de un usuario.
idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

-- Devuelve el nombre de un usuario.
nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

-- Devuelve el usuario de una publicaci on.
usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

-- Devuelve el conjunto de usuarios que le dieron me gusta a una publicaci on.
likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

--------------------------------------------------- Ejercicios

----------------------------------------------------------------------------------------------------

-- Dada una red, devuelve una lista con sus nombres de usuarios.
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = proyectarNombres (usuarios red)

-- Dada una lista de usuarios, evuelve una lista sus nombres sin repeticiones.
proyectarNombres :: [Usuario] -> [String]
proyectarNombres us = eliminarRepetidos (listaDeNombres us)

-- Dada una lista de usuarios, devuelve una lista con sus nombres.
listaDeNombres :: [Usuario] -> [String]
listaDeNombres [] = []
listaDeNombres us = un : listaDeNombres ust
                    where un = nombreDeUsuario (head(us))
                          ust = tail(us)

-- Dada una lista, devuelve la lista sin repetición de elementos.
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | pertenece x xs = x : (eliminarRepetidos (quitarTodos x xs))
                         | otherwise = x : (eliminarRepetidos xs)

-- Dada una lista de elementos y un elemento, evuelve una lista con ninguna aparición de ese elemento.
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos e [] = []
quitarTodos e (x:xs) | pertenece e (x:xs) = quitarTodos e (quitar e (x:xs))
                     | otherwise = (x:xs)

-- Dada una lista de elementos y un elemento, devuelve la lista sin la primera aparición de ese elemento.
quitar :: (Eq t) => t -> [t] -> [t]
quitar e [] = []
quitar e (x:xs) | e == x = xs
                | pertenece e xs = x:(quitar e xs)
                | otherwise = (x:xs)

----------------------------------------------------------------------------------------------------

-- Dada una red y un usuario, devuelve una lista con todos los usuarios con los que se relaciona el usuario en esa red.
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u = amigosDeEnListaDeRelaciones (relaciones red) u

-- Dada una lista de relaciones y un usuario, devuelve una lista con todos los usuarios con los que se relaciones el usuario.
amigosDeEnListaDeRelaciones :: [Relacion] -> Usuario -> [Usuario]
amigosDeEnListaDeRelaciones [] u = []
amigosDeEnListaDeRelaciones rels u | pupr == u = supr : amigosDeEnListaDeRelaciones (tail(rels)) u
                                   | supr == u = pupr : amigosDeEnListaDeRelaciones (tail(rels)) u
                                   | otherwise = amigosDeEnListaDeRelaciones (tail(rels)) u
                                     where pupr = fst(head(rels))
                                           supr = snd(head(rels))

----------------------------------------------------------------------------------------------------

-- Dada una red y un usuario, devuelve la longitud de la lista de amigos del usuario en esa red.
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = fromInteger(longitud (amigosDe red u))

----------------------------------------------------------------------------------------------------

-- 
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = compararCantidadDeAmigos red (usuarios red)

compararCantidadDeAmigos :: RedSocial -> [Usuario] -> Usuario
compararCantidadDeAmigos red (x:[]) = x
compararCantidadDeAmigos red us | cantidadDeAmigos red u1 <= cantidadDeAmigos red u2 = compararCantidadDeAmigos red (tail(us))
                                | otherwise = compararCantidadDeAmigos red (quitar u2 us)
                                  where u1 = head(us)
                                        u2 = head(tail(us))

----------------------------------------------------------------------------------------------------

-- Dada una red, valida si hay un usuario con más de un millón de amigos.
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = (cantidadDeAmigos red (usuarioConMasAmigos red)) > 1000000000

----------------------------------------------------------------------------------------------------

-- Dada una red y un usuario, devuelve la lista de todas las publicaciones del usuario en esa red.
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u = filtrarPublicacionesPorUsuario u (publicaciones red)

-- Dado un usuario y una lista de publicaciones, devuelve la lista de todas las publicaciones que sean del usuario en la lista de publicaciones.
filtrarPublicacionesPorUsuario :: Usuario -> [Publicacion] -> [Publicacion]
filtrarPublicacionesPorUsuario _ [] = []
filtrarPublicacionesPorUsuario u pubs | u == upub = pub : (filtrarPublicacionesPorUsuario u pubsres)
                                      | otherwise = filtrarPublicacionesPorUsuario u pubsres
                                        where pub = head(pubs)
                                              pubsres = tail(pubs)
                                              upub = usuarioDePublicacion pub

----------------------------------------------------------------------------------------------------

-- Dada una red y un usuario, devuelve la lista de todas las publicaciones a las cuales el usuario les puso like en esa red.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u = filtrarPublicacionesConLikeDeUsuario u (publicaciones red)  

-- Dado un usuario y una lista de publicaciones, devuelve la lista de todas publicaciones a las que el usuarios les puso like en la lista de publicaciones.
filtrarPublicacionesConLikeDeUsuario :: Usuario -> [Publicacion] -> [Publicacion]
filtrarPublicacionesConLikeDeUsuario _ [] = []
filtrarPublicacionesConLikeDeUsuario u pubs | (pertenece u likespub) = pub : (filtrarPublicacionesConLikeDeUsuario u pubsres)
                                            | otherwise = filtrarPublicacionesConLikeDeUsuario u pubsres
                                              where pub = head(pubs)
                                                    pubsres = tail(pubs)
                                                    likespub = likesDePublicacion pub

----------------------------------------------------------------------------------------------------

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined

-------------------------------------------------- Predicados

----------------------------------------------------------------------------------------------------

-- Valida que un elemento pertenece a una lista.
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece e [] = False
pertenece e l = e == hl || pertenece e tl
                where hl = head(l)
                      tl = tail(l)

----------------------------------------------------------------------------------------------------

-- Valida que ambas listas tienen la misma longitud y los mismos elementos, sin importar las repeticiones.
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos l1 l2 = (longitud l1 == longitud l2) && (todoElemPertenece l1 l2) && (todoElemPertenece l2 l1)

-- Devuelve la longitud de una lista.
longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- Valida que todo elemento de la primer lista pertenece a la segunda.
todoElemPertenece :: (Eq t) => [t] -> [t] -> Bool
todoElemPertenece [] b = True
todoElemPertenece (x:xs) b = (pertenece x b) && (todoElemPertenece xs b)

----------------------------------------------------------------------------------------------------

-- Valida que una red social sea válida cuando tiene usuarios, relaciones y publicaciones válidas.
redSocialValida :: RedSocial -> Bool
redSocialValida red = (usuariosValidos (usuarios red)) && (relacionesValidas (usuarios red) (relaciones red)) && (publicacionesValidas (usuarios red) (publicaciones red))

----------------------------------------------------------------------------------------------------

-- Valida que todos los ususarios de la lista sean válidos y no haya ids repetidos
usuariosValidos :: [Usuario] -> Bool
usuariosValidos [] = True
usuariosValidos us = ((usuarioValido u) && (usuariosValidos ust)) && (noHayIdsRepetidos us)
                     where u = head(us)
                           ust = tail(us)

----------------------------------------------------------------------------------------------------

-- Valida el ID y nombre de usuario, en el primer caso siendo mayor a cero y en el segundo siendo su longitud mayor a cero.
usuarioValido :: Usuario -> Bool
usuarioValido u = ((idDeUsuario u) > 0) && ((longitud (nombreDeUsuario u)) > 0)

----------------------------------------------------------------------------------------------------

-- Valida que no hay ids repetidos en una lista de usuarios.
noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos [] = True
noHayIdsRepetidos us = sinRepetidos (idsDeUsuarios us)

-- Devuelve una lista con los ids de una lista de usuarios.
idsDeUsuarios :: [Usuario] -> [Integer]
idsDeUsuarios [] = []
idsDeUsuarios (x:xs) = (idDeUsuario x) : idsDeUsuarios xs

----------------------------------------------------------------------------------------------------

-- Valida que todas las relaciones sean válidas tanto porque los usuarios existan como porque las relaciones sean únicas.
relacionesValidas :: [Usuario] -> [Relacion] -> Bool
relacionesValidas us rels = (usuariosDeRelacionValidos us rels) && (relacionesAsimetricas rels) && (noHayRelacionesRepetidas rels)

----------------------------------------------------------------------------------------------------

-- Valida que si hay una relación entre ususarios, ambos pertenecen a la lista de usuarios y no son el mismo
usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool
usuariosDeRelacionValidos us [] = True
usuariosDeRelacionValidos us rels = (u1 /= u2 && pertenece u1 us && pertenece u2 us) && (usuariosDeRelacionValidos us (tail(rels)))
                                    where u1 = fst(head(rels))
                                          u2 = snd(head(rels))

----------------------------------------------------------------------------------------------------

-- Valida que no hay relaciones simétricas
relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas [] = True
relacionesAsimetricas rels = not (pertenece (u2, u1) rels) && relacionesAsimetricas (tail(rels))
                             where u1 = fst(head(rels))
                                   u2 = snd(head(rels))

----------------------------------------------------------------------------------------------------

-- Valida que no haya relaciones duplicadas
noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas [] = True
noHayRelacionesRepetidas rels = sinRepetidos(idsDeUsuariosRelacionados(rels))

-- Dada una lista de relaciones, devuelve una lista de relaciones reemplazando cada usuario por su id.
idsDeUsuariosRelacionados :: [Relacion] -> [(Integer, Integer)]
idsDeUsuariosRelacionados [] = []
idsDeUsuariosRelacionados rels = (idu1, idu2) : idsDeUsuariosRelacionados (tail(rels))
                                 where idu1 = idDeUsuario(fst(head(rels)))
                                       idu2 = idDeUsuario(snd(head(rels)))

----------------------------------------------------------------------------------------------------

-- Valida que las publicaciones en una lista de pubicaciones son válidas, chequeando los usuarios que las hicieron, los usuarios que les dieron like y que no estén duplicadas.
publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas us pubs = (usuariosDePublicacionSonUsuariosDeRed us pubs) && (usuariosDeLikeDePublicacionesSonUsuariosDeRed us pubs) && (noHayPublicacionesRepetidas pubs)

----------------------------------------------------------------------------------------------------

-- Valida que las publicaciones están hechas por ususarios de una lista de usuarios
usuariosDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDePublicacionSonUsuariosDeRed us [] = True
usuariosDePublicacionSonUsuariosDeRed us pubs = (pertenece up us) && (usuariosDePublicacionSonUsuariosDeRed us (tail(pubs)))
                                                where up = usuarioDePublicacion(head(pubs))

----------------------------------------------------------------------------------------------------

-- Valida que todos los likes de cada publicación en una lista de publicaciones, son de usuarios que se encuentran en una lista de usuarios dada.
usuariosDeLikeDePublicacionesSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDeLikeDePublicacionesSonUsuariosDeRed us [] = True
usuariosDeLikeDePublicacionesSonUsuariosDeRed us pubs = (usuariosLikeValidos us usl) && (usuariosDeLikeDePublicacionesSonUsuariosDeRed us (tail(pubs)))
                                                        where usl = likesDePublicacion(head(pubs))

----------------------------------------------------------------------------------------------------

-- Valida que todo usuario de la segunda lista de usuarios se encuentra en la primera.
usuariosLikeValidos :: [Usuario] -> [Usuario] -> Bool
usuariosLikeValidos us usl = todoElemPertenece usl us

----------------------------------------------------------------------------------------------------

-- Valida que no hay publicaciones repetidas
noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas [] = True
noHayPublicacionesRepetidas pubs = sinRepetidos(idsDeUsuariosYPublicaciones(pubs))

-- Armar una lista con los ids de ususarios y los textos de las publicaciones
idsDeUsuariosYPublicaciones :: [Publicacion] -> [(Integer, String)]
idsDeUsuariosYPublicaciones [] = []
idsDeUsuariosYPublicaciones pubs = (id, txt) : idsDeUsuariosYPublicaciones (tail(pubs))
                                   where id = idDeUsuario(usuarioDePublicacion(head(pubs)))
                                         txt = contenidoDePublicacion(head(pubs))

-- Devuelve el texto de una publicación
contenidoDePublicacion :: Publicacion -> String
contenidoDePublicacion (_, txt, _) = txt

----------------------------------------------------------------------------------------------------

-- Valida si dada una lista de usuarios, cada uno se relaciona con el siguiente en una red.
cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos (x:[]) red = True
cadenaDeAmigos us red = (relacionadosDirecto u1 u2 red) && (cadenaDeAmigos ust red)
                        where u1 = head(us)
                              u2 = head(tail(us))
                              ust = tail(us)

----------------------------------------------------------------------------------------------------

-- Valida si dos usuarios se relacionan en una red social
relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto u1 u2 red = (pertenece (u1, u2) (relaciones(red))) || (pertenece (u2, u1) (relaciones(red)))

----------------------------------------------------------------------------------------------------

-- Valida si todos los usuarios en una lista perteneces a una red.
sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed red [] = True
sonDeLaRed red us = (pertenece u usred) && (sonDeLaRed red ust)
                    where u = head(us)
                          ust = tail(us)
                          usred = usuarios(red)

----------------------------------------------------------------------------------------------------

-- Valida si una lista de cualquier tipo empieza con un elemento dado del mismo tipo (la comparación se hace entre elementos). 
empiezaCon :: (Eq t) => t -> [t] -> Bool
empiezaCon e l = head(l) == e

----------------------------------------------------------------------------------------------------

-- Valida si una lista de cualquier tipo termina con un elemento dado del mismo tipo (la comparación se hace entre listas de elementos).
terminaCon :: (Eq t) => t -> [t] -> Bool
terminaCon e [] = False
terminaCon e (x:[]) = [x] == [e] 
terminaCon e l = terminaCon e (tail(l)) 

----------------------------------------------------------------------------------------------------

-- Valida que todos los elementos de una lista sean distintos.
sinRepetidos :: (Eq t) => [t] -> Bool
sinRepetidos [] = True
sinRepetidos l = not (pertenece hl tl) && (sinRepetidos tl)
                 where hl = head(l)
                       tl = tail(l)

----------------------------------------------------------------------------------------------------

-------------------------------------------------- Variables de Tests 

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Juan")
usuario7 = (7, "Carlos")
usuario8 = (8, "Pepe")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) 
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])

usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

usuariosC = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8]