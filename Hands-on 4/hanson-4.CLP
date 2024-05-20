(deftemplate Smartphone
   (slot id)
   (slot marca)
   (slot modelo)
   (slot precio)
   (slot stock))

(deftemplate Computadora
   (slot id)
   (slot tipo) ; tipo puede ser "laptop" o "desktop"
   (slot marca)
   (slot modelo)
   (slot precio)
   (slot stock))

(deftemplate Accesorio
   (slot id)
   (slot tipo) ; tipo puede ser "funda", "cargador", "audífonos", etc.
   (slot marca)
   (slot precio)
   (slot stock))

(deftemplate Cliente
   (slot id)
   (slot nombre)
   (slot direccion)
   (slot telefono)
   (slot email))

(deftemplate OrdenDeCompra
   (slot id)
   (slot cliente-id)
   (multislot items) ; lista de items, cada uno puede ser un id de smartphone, computadora o accesorio
   (slot total)
   (slot fecha))

(deftemplate TarjetaDeCredito
   (slot id)
   (slot cliente-id)
   (slot numero)
   (slot vencimiento)
   (slot cvv))

(deftemplate Vale
   (slot id)
   (slot cliente-id)
   (slot monto)
   (slot fecha-expiracion))


; Hechos sobre Smartphones
(assert (Smartphone (id 1) (marca "Apple") (modelo "iPhone 14") (precio 999.99) (stock 50)))
(assert (Smartphone (id 2) (marca "Samsung") (modelo "Galaxy S22") (precio 899.99) (stock 30)))

; Hechos sobre Computadores
(assert (Computadora (id 1) (tipo "laptop") (marca "Dell") (modelo "XPS 13") (precio 1199.99) (stock 20)))
(assert (Computadora (id 2) (tipo "desktop") (marca "HP") (modelo "Omen 30L") (precio 1599.99) (stock 15)))

; Hechos sobre Accesorios
(assert (Accesorio (id 1) (tipo "funda") (marca "Spigen") (precio 19.99) (stock 100)))
(assert (Accesorio (id 2) (tipo "cargador") (marca "Anker") (precio 29.99) (stock 200)))

; Hechos sobre Clientes
(assert (Cliente (id 1) (nombre "Juan Pérez") (direccion "Calle Falsa 123") (telefono "555-1234") (email "juan.perez@example.com")))
(assert (Cliente (id 2) (nombre "María Gómez") (direccion "Avenida Siempre Viva 456") (telefono "555-5678") (email "maria.gomez@example.com")))

; Hechos sobre Orden de Compra
(assert (OrdenDeCompra (id 1) (cliente-id 1) (items (create$ 1 2)) (total 1899.98) (fecha "2024-05-19")))
(assert (OrdenDeCompra (id 2) (cliente-id 2) (items (create$ 3 4)) (total 49.98) (fecha "2024-05-19")))

; Hechos sobre Tarjetas de Crédito
(assert (TarjetaDeCredito (id 1) (cliente-id 1) (numero "1234-5678-9123-4567") (vencimiento "12/25") (cvv "123")))
(assert (TarjetaDeCredito (id 2) (cliente-id 2) (numero "9876-5432-1098-7654") (vencimiento "11/24") (cvv "456")))

; Hechos sobre Vales
(assert (Vale (id 1) (cliente-id 1) (monto 50.00) (fecha-expiracion "2024-12-31")))
(assert (Vale (id 2) (cliente-id 2) (monto 75.00) (fecha-expiracion "2024-11-30")))

; Regla 1: Aplicar descuento a smartphones si el stock es mayor a 40
(defrule aplicar-descuento-smartphones
    (Smartphone (id ?id) (stock ?stock&:(> ?stock 40)))
    =>
    (printout t "Aplicando descuento al smartphone con id: " ?id crlf))

; Regla 2: Notificar cuando un computador tiene stock bajo
(defrule notificar-stock-bajo-computadoras
    (Computadora (id ?id) (stock ?stock&:(< ?stock 10)))
    =>
    (printout t "Notificación: El computador con id " ?id " tiene stock bajo (" ?stock ")" crlf))

; Regla 3: Recomendar accesorio si el cliente compra un smartphone
(defrule recomendar-accesorio
    (OrdenDeCompra (cliente-id ?cliente-id) (items $?items (create$ ?smartphone-id&:(>= ?smartphone-id 1) <=?smartphone-id 2) $?))
    (Accesorio (id ?accesorio-id) (tipo "funda"))
    =>
    (printout t "Recomendar funda al cliente " ?cliente-id " para el smartphone " ?smartphone-id crlf))

; Regla 4: Validar tarjeta de crédito antes de procesar orden
(defrule validar-tarjeta
    (OrdenDeCompra (id ?orden-id) (cliente-id ?cliente-id))
    (not (TarjetaDeCredito (cliente-id ?cliente-id)))
    =>
    (printout t "Error: Cliente " ?cliente-id " no tiene una tarjeta de crédito válida para la orden " ?orden-id crlf))

; Regla 5: Enviar recordatorio de vale por expirar
(defrule recordatorio-vale
    (Vale (id ?vale-id) (fecha-expiracion ?fecha))
    (test (> (str-compare ?fecha "2024-06-01") 0))
    =>
    (printout t "Recordatorio: El vale " ?vale-id " está próximo a expirar el " ?fecha crlf))

; Regla 6: Aplicar vale al total de la orden
(defrule aplicar-vale
    (OrdenDeCompra (id ?orden-id) (cliente-id ?cliente-id) (total ?total))
    (Vale (cliente-id ?cliente-id) (monto ?monto))
    =>
    (bind ?nuevo-total (- ?total ?monto))
    (printout t "Aplicando vale de " ?monto " al total de la orden " ?orden-id ". Nuevo total: " ?nuevo-total crlf))

; Regla 7: Notificar cliente si la dirección está incompleta
(defrule notificar-direccion-incompleta
    (Cliente (id ?id) (direccion ?direccion&:(< (length$ ?direccion) 10)))
    =>
    (printout t "Notificación: La dirección del cliente " ?id " está incompleta." crlf))

; Regla 8: Ofrecer promoción para clientes nuevos
(defrule promocion-clientes-nuevos
    (Cliente (id ?id) (email ?email))
    (not (OrdenDeCompra (cliente-id ?id)))
    =>
    (printout t "Ofrecer promoción al nuevo cliente: " ?id crlf))

; Regla 9: Verificar stock antes de procesar orden de compra
(defrule verificar-stock
    (OrdenDeCompra (id ?orden-id) (items $?items))
    (or
        (Smartphone (id ?item-id) (stock ?stock&:(< ?stock 1)))
        (Computadora (id ?item-id) (stock ?stock&:(< ?stock 1)))
        (Accesorio (id ?item-id) (stock ?stock&:(< ?stock 1))))
    =>
    (printout t "Error: El item " ?item-id " no tiene suficiente stock para la orden " ?orden-id crlf))

; Regla 10: Notificar cliente cuando su orden es enviada
(defrule notificar-envio
    (OrdenDeCompra (id ?orden-id) (cliente-id ?cliente-id))
    =>
    (printout t "Notificación: La orden " ?orden-id " del cliente " ?cliente-id " ha sido enviada." crlf))

; Regla 11: Aplicar un descuento a las laptops
(defrule descuento-laptops
    (Computadora (id ?id) (tipo "laptop") (precio ?precio))
    =>
    (bind ?nuevo-precio (* ?precio 0.9))
    (printout t "Aplicando descuento a la laptop " ?id ". Nuevo precio: " ?nuevo-precio crlf))

; Regla 12: Notificar si un cliente hace su primera compra
(defrule primera-compra
    (OrdenDeCompra (id ?orden-id) (cliente-id ?cliente-id))
    (not (exists (OrdenDeCompra (cliente-id ?cliente-id) (id ?otra-orden-id&:(< ?otra-orden-id ?orden-id)))))
    =>
    (printout t "Notificación: El cliente " ?cliente-id " ha realizado su primera compra. Orden ID: " ?orden-id crlf))

; Regla 13: Enviar agradecimiento por compras superiores a $1000
(defrule agradecimiento-compras-grandes
    (OrdenDeCompra (id ?orden-id) (total ?total&:(>= ?total 1000)))
    =>
    (printout t "Agradecimiento: Gracias por su compra de " ?total " en la orden " ?orden-id crlf))

; Regla 14: Verificar validez de la fecha de vencimiento de la tarjeta de crédito
(defrule verificar-vencimiento-tarjeta
    (TarjetaDeCredito (id ?tarjeta-id) (vencimiento ?vencimiento&:(str-compare ?vencimiento "2024-01-01") <= 0))
    =>
    (printout t "Error: La tarjeta de crédito " ?tarjeta-id " ha expirado." crlf))

; Regla 15: Notificar cuando un cliente tiene múltiples vales
(defrule multiples-vales
    (Cliente (id ?cliente-id))
    (test (>= (length$ (find-all-facts ((?f Vale)) (eq ?f:cliente-id ?cliente-id))) 2))
    =>
    (printout t "Notificación: El cliente " ?cliente-id " tiene múltiples vales." crlf))

; Regla 16: Enviar un mensaje de bienvenida a nuevos clientes
(defrule bienvenida-clientes
    (Cliente (id ?id))
    (not (exists (OrdenDeCompra (cliente-id ?id))))
    =>
    (printout t "Bienvenida: Bienvenido al sistema, cliente " ?id "." crlf))

; Regla 17: Alertar sobre tarjetas de crédito con CVV inválido
(defrule cvv-invalido
    (TarjetaDeCredito (id ?tarjeta-id) (cvv ?cvv&:(< (length$ (str-cat ?cvv)) 3)))
    =>
    (printout t "Alerta: La tarjeta de crédito " ?tarjeta-id " tiene un CVV inválido." crlf))

; Regla 18: Recomendar accesorios populares
(defrule recomendar-accesorios-populares
    (Accesorio (id ?id) (stock ?stock&:(> ?stock 150)))
    =>
    (printout t "Recomendación: El accesorio " ?id " es muy popular." crlf))

; Regla 19: Enviar alerta por clientes con órdenes pendientes
(defrule alerta-ordenes-pendientes
    (OrdenDeCompra (cliente-id ?cliente-id) (id ?orden-id))
    (test (>= (length$ (find-all-facts ((?f OrdenDeCompra)) (eq ?f:cliente-id ?cliente-id))) 3))
    =>
    (printout t "Alerta: El cliente " ?cliente-id " tiene múltiples órdenes pendientes." crlf))

; Regla 20: Aplicar descuento a ordenes de clientes con tarjeta de crédito activa
(defrule descuento-tarjeta-activa
    (OrdenDeCompra (id ?orden-id) (cliente-id ?cliente-id) (total ?total))
    (TarjetaDeCredito (cliente-id ?cliente-id) (vencimiento ?vencimiento&:(str-compare ?vencimiento "2024-05-01") > 0))
    =>
    (bind ?nuevo-total (* ?total 0.95))
    (printout t "Aplicando descuento del 5% a la orden " ?orden-id " del cliente " ?cliente-id ". Nuevo total: " ?nuevo-total crlf))

