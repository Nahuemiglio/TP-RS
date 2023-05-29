module Solucion where

-- Nombre de Grupo: chachipiti

-- Integrante 1: Miglioranza Da Silva Nahuel, nahuemiglio@gmail.com, 279/19
-- Integrante 2: Briccola Francina, franbriccola2309@gmail.com, 1582/21
-- Integrante 3: Mercado Ernesto Josue, mercadoernesto645@gmail.com, 174/22
-- Integrante 4: Tejerina Augusto Damián, damiantgrina@gmail.com, 708/23

--------------------------------------------------- Declaración de tipos

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

-- Devuelve el n ́umero de identificación de un usuario.
idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

-- Devuelve el nombre de un usuario.
nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

-- Devuelve el usuario de una publicación.
usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

-- Devuelve el conjunto de usuarios que le dieron me gusta a una publicación.
likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

--------------------------------------------------- Ejercicios

--------------------------------------------------- Problema 1

-- Dada una red, devuelve una lista con sus nombres de usuarios.
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = proyectarNombres (usuarios red)

-- Dada una lista de usuarios, devuelve una lista sus nombres sin repeticiones.
proyectarNombres :: [Usuario] -> [String]
proyectarNombres [] = []
proyectarNombres us = agregarSinRepetidos nu (proyectarNombres usres)
                      where nu = nombreDeUsuario (head(us))
                            usres = tail(us)

-- Dado un string 'nu' y una lista de strings 'nus', agrega el string 'nu' a la lista de strings 'nus' si 'nu' no pertenecía a 'nus'.
-- En este caso 'nu' será un nombre de usuario y 'nus' será una lista de nombres de usuarios, por lo que en este caso se agregará el nombre de usuario a la lista de nombres de usuarios si el nombre de usuario no pertenecía a la lista.
agregarSinRepetidos :: String -> [String] -> [String]
agregarSinRepetidos nu [] = [nu]
agregarSinRepetidos nu nus | pertenece nu nus = nus
                           | otherwise = nu : nus 

--------------------------------------------------- Problema 2

-- Dada una red y un usuario, devuelve una lista con todos los usuarios con los que se relaciona el usuario en esa red.
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u = amigosDeEnListaDeRelaciones (relaciones red) u

-- Dada una lista de relaciones y un usuario, devuelve una lista con todos los usuarios con los que se relaciones el usuario.
amigosDeEnListaDeRelaciones :: [Relacion] -> Usuario -> [Usuario]
amigosDeEnListaDeRelaciones [] u = []
amigosDeEnListaDeRelaciones rels u | u1 == u = u2 : amigosDeEnListaDeRelaciones (tail(rels)) u
                                   | u2 == u = u1 : amigosDeEnListaDeRelaciones (tail(rels)) u
                                   | otherwise = amigosDeEnListaDeRelaciones (tail(rels)) u
                                     where u1 = fst(head(rels))
                                           u2 = snd(head(rels))

{-
-- Dada una lista de relaciones y un usuario, devuelve una lista con todos los usuarios con los que se relaciones el usuario.
amigosDeEnListaDeRelaciones :: [Relacion] -> Usuario -> [Usuario]
amigosDeEnListaDeRelaciones [] u = []
amigosDeEnListaDeRelaciones rels (fst(head(rels))) = snd(head(rels)) : amigosDeEnListaDeRelaciones (tail(rels)) (fst(head(rels)))
amigosDeEnListaDeRelaciones rels (snd(head(rels))) = fst(head(rels)) : amigosDeEnListaDeRelaciones (tail(rels)) (snd(head(rels)))
amigosDeEnListaDeRelaciones rels u = amigosDeEnListaDeRelaciones (tail(rels)) u
-}
--------------------------------------------------- Problema 3

-- Dada una red y un usuario, devuelve la longitud de la lista de amigos del usuario en esa red.
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = fromInteger(longitud(amigosDe red u))

--------------------------------------------------- Problema 4

-- Dada una red social, devuelve el usuario con más amigos en la misma.
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = compararCantidadDeAmigos red (usuarios red)

-- Dada una red social y una lista de usuarios, devuelve el usuario con más relaciones (más amigos) en la lista de usuarios.
compararCantidadDeAmigos :: RedSocial -> [Usuario] -> Usuario
compararCantidadDeAmigos red (x:[]) = x
compararCantidadDeAmigos red us | cantidadDeAmigos red u1 <= cantidadDeAmigos red u2 = compararCantidadDeAmigos red (tail(us))
                                | otherwise = compararCantidadDeAmigos red (u1:tail(tail(us)))
                                  where u1 = head(us)
                                        u2 = head(tail(us))

--------------------------------------------------- Problema 5

-- Dada una red, valida si hay un usuario con más de diez de amigos.
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = (cantidadDeAmigos red (usuarioConMasAmigos red)) > 10

--------------------------------------------------- Problema 6

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

--------------------------------------------------- Problema 7

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

--------------------------------------------------- Problema 8

-- Dada una red y dos usuarios verifica si ambos le dieron like a las mismas publicaciones.
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)

--------------------------------------------------- Problema 9

-- Dada una red y un usuario, verifica si hay alguien en la lista de usuarios de la red que le haya dado like a todas las publicaciones en la red del usuario. 
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u = algunoLeDioLikeATodasPublicaciones red (publicacionesDe red u) (usuarios red) 

-- Dada una red, una lista de publicaciones y una lista de usuarios, verifica si toda publicación de la lista de publicaciones pertenece a la lista de publicaciones que le gustan a algún usuario en la lista de usuarios.
algunoLeDioLikeATodasPublicaciones :: RedSocial -> [Publicacion] -> [Usuario] -> Bool
algunoLeDioLikeATodasPublicaciones _ _ [] = False
algunoLeDioLikeATodasPublicaciones _ [] _ = False
algunoLeDioLikeATodasPublicaciones red pubs us = ((usuarioDePublicacion(head(pubs)) /= u) && (todoElemPertenece pubs (publicacionesQueLeGustanA red u))) || (algunoLeDioLikeATodasPublicaciones red pubs usres)
                                                 where u = head(us)
                                                       usres = tail(us)

--------------------------------------------------- Problema 10

-- Dada una red y dos usuarios (u1 y u2), valida si ambos usuarios pertenecen a un mismo grupo. 
-- Definimos grupo como una lista de usuarios donde es posible obtener una cadenaDeAmigos entre dos usuarios cualesquiera de ese grupo, formada dicha cadena con usuarios de esa lista.
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 = pertenecenAlMismoGrupo grupos u1 u2 
                                    where grupos = agrupar (relaciones red)

-- Dada una lista de grupos y dos usuarios (u1 y u2), valida si ambos usuarios pertenecen al mismo grupo.
pertenecenAlMismoGrupo :: [[Usuario]] -> Usuario -> Usuario -> Bool
pertenecenAlMismoGrupo [] u1 u2 = False
pertenecenAlMismoGrupo grupos u1 u2 = ((pertenece u1 grupo) && (pertenece u2 grupo)) || pertenecenAlMismoGrupo gruposres u1 u2 
                                      where grupo = head(grupos)
                                            gruposres = tail(grupos)

-- Dada una lista de relaciones, añade a todo usuario de cada relación al grupo en el cual se relacione con alguno de sus miembros.
agrupar :: [Relacion] -> [[Usuario]]
agrupar [] = []
agrupar rels = agregarAlGrupo (agrupar relsres) rel 
                      where rel = head(rels)
                            relsres = tail(rels)

-- Dada una lista de grupos y una relación, agrega alguno de los usuarios de la relación a un grupo si ahí está el otro usuario de la relación, si no crea un nuevo grupo con ambos usuarios de la relación.
agregarAlGrupo :: [[Usuario]] -> Relacion -> [[Usuario]]
agregarAlGrupo [] (u1, u2) = [[u1, u2]]
agregarAlGrupo grupos rel | pertenece u1 grupo = (u2:grupo) : gruposres
                          | pertenece u2 grupo = (u1:grupo) : gruposres
                          | otherwise = grupo : (agregarAlGrupo gruposres rel)
                            where grupo = head(grupos)
                                  gruposres = tail(grupos)
                                  u1 = fst(rel)
                                  u2 = snd(rel)

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