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
