; Definir plantillas para los hechos
(deftemplate estaEn
   (slot objeto)
   (slot ubicacion))

(deftemplate manosLibres
   (slot mono))

(deftemplate manosOcupadas
   (slot mono))

(deftemplate Hambriento
   (slot mono))

(deftemplate Comer
   (slot mono)
   (slot platano))

(deftemplate Satisfecho
   (slot mono))

(deftemplate elegirAccion
   (slot accion))

; Inicializar hechos
(deffacts estado-inicial
  (Hambriento (mono mono))
  (estaEn (objeto mesa) (ubicacion suelo))
  (estaEn (objeto platano) (ubicacion techo-centro))
  (estaEn (objeto mono) (ubicacion suelo))
  (manosLibres (mono mono)))

; Reglas para elegir aleatoriamente la siguiente acción
(defrule elegir-accion
  (not (elegirAccion))
  =>
  (bind ?accion (nth$ (random 1 7) (create$ "mover-mono-a-mesa" "mover-mono-izquierda" "mover-mono-derecha" "agarrar-mesa" "subir-mesa" "mover-mesa-al-centro" "soltar-mesa")))
  (assert (elegirAccion (accion ?accion)))
  (printout t "Eligiendo acción: " ?accion crlf))

; Reglas generales para simular las acciones del mono

(defrule mover-mono-a-mesa
  (elegirAccion (accion mover-mono-a-mesa))
  ?mono <- (estaEn (objeto mono) (ubicacion suelo))
  (not (estaEn (objeto mono) (ubicacion cerca-de-la-mesa)))
  =>
  (modify ?mono (ubicacion cerca-de-la-mesa))
  (retract (elegirAccion (accion mover-mono-a-mesa)))
  (printout t "El mono se mueve cerca de la mesa" crlf))

(defrule mover-mono-izquierda
  (elegirAccion (accion mover-mono-izquierda))
  ?mono <- (estaEn (objeto mono) (ubicacion suelo))
  (not (estaEn (objeto mono) (ubicacion cerca-de-la-mesa)))
  =>
  (modify ?mono (ubicacion cerca-de-la-pared))
  (retract (elegirAccion (accion mover-mono-izquierda)))
  (printout t "El mono se mueve cerca de la pared a la izquierda" crlf))

(defrule mover-mono-derecha
  (elegirAccion (accion mover-mono-derecha))
  ?mono <- (estaEn (objeto mono) (ubicacion suelo))
  (not (estaEn (objeto mono) (ubicacion cerca-de-la-mesa)))
  =>
  (modify ?mono (ubicacion cerca-de-la-ventana))
  (retract (elegirAccion (accion mover-mono-derecha)))
  (printout t "El mono se mueve cerca de la ventana a la derecha" crlf))

(defrule agarrar-mesa
  (elegirAccion (accion agarrar-mesa))
  ?manosLibres <- (manosLibres (mono mono))
  (estaEn (objeto mesa) (ubicacion suelo))
  (estaEn (objeto mono) (ubicacion cerca-de-la-mesa))
  =>
  (retract ?manosLibres)
  (assert (manosOcupadas (mono mono)))
  (retract (elegirAccion (accion agarrar-mesa)))
  (printout t "El mono agarra la mesa" crlf))

(defrule mover-mesa-al-centro
  (elegirAccion (accion mover-mesa-al-centro))
  ?manosOcupadas <- (manosOcupadas (mono mono))
  ?mesa <- (estaEn (objeto mesa) (ubicacion suelo))
  =>
  (retract ?mesa)
  (assert (estaEn (objeto mesa) (ubicacion centro)))
  (retract (elegirAccion (accion mover-mesa-al-centro)))
  (printout t "Mover la mesa al centro" crlf))

(defrule soltar-mesa
  (elegirAccion (accion soltar-mesa))
  ?manosOcupadas <- (manosOcupadas (mono mono))
  (estaEn (objeto mesa) (ubicacion centro))
  =>
  (retract ?manosOcupadas)
  (assert (manosLibres (mono mono)))
  (retract (elegirAccion (accion soltar-mesa)))
  (printout t "El mono suelta la mesa y se mueve al suelo" crlf))

(defrule subir-mesa
  (elegirAccion (accion subir-mesa))
  ?mono <- (estaEn (objeto mono) (ubicacion suelo))
  (estaEn (objeto mesa) (ubicacion centro))
  =>
  (modify ?mono (ubicacion mesa))
  (retract (elegirAccion (accion subir-mesa)))
  (printout t "El mono se sube a la mesa" crlf))

(defrule agarrar-y-comer-platano
  (elegirAccion (accion agarrar-y-comer-platano))
  ?mono <- (estaEn (objeto mono) (ubicacion mesa))
  ?manosLibres <- (manosLibres (mono mono))
  =>
  (retract ?manosLibres)
  (assert (manosOcupadas (mono mono)))
  (assert (Comer (mono mono) (platano platano)))
  (retract (elegirAccion (accion agarrar-y-comer-platano)))
  (printout t "El mono agarra y come el plátano" crlf))

(defrule mono-satisfecho
  (Comer (mono mono) (platano platano))
  ?hambriento <- (Hambriento (mono mono))
  =>
  (retract ?hambriento)
  (assert (Satisfecho (mono mono)))
  (printout t "El mono está satisfecho" crlf)
  (halt))

(defrule mover-mono-atras
  (elegirAccion (accion mover-mono-atras))
  ?mono <- (estaEn (objeto mono) (ubicacion cerca-de-la-mesa))
  =>
  (modify ?mono (ubicacion suelo))
  (retract (elegirAccion (accion mover-mono-atras)))
  (printout t "El mono se mueve de vuelta al suelo" crlf))

(defrule mover-mesa-atras
  (elegirAccion (accion mover-mesa-atras))
  ?manosOcupadas <- (manosOcupadas (mono mono))
  (estaEn (objeto mesa) (ubicacion centro))
  =>
  (retract ?manosOcupadas)
  (assert (estaEn (objeto mesa) (ubicacion suelo)))
  (retract (elegirAccion (accion mover-mesa-atras)))
  (printout t "Mover la mesa de vuelta al suelo" crlf))

(defrule mono-descansa
  (elegirAccion (accion mono-descansa))
  ?mono <- (estaEn (objeto mono) (ubicacion mesa))
  (manosOcupadas (mono mono))
  =>
  (retract (manosOcupadas (mono mono)))
  (assert (manosLibres (mono mono)))
  (retract (elegirAccion (accion mono-descansa)))
  (printout t "El mono descansa en la mesa" crlf))

(defrule mono-se-para
  (elegirAccion (accion mono-se-para))
  ?mono <- (estaEn (objeto mono) (ubicacion suelo))
  (not (manosOcupadas (mono mono)))
  =>
  (retract (elegirAccion (accion mono-se-para)))
  (printout t "El mono se para en el suelo" crlf))

(defrule mono-camina
  (elegirAccion (accion mono-camina))
  ?mono <- (estaEn (objeto mono) (ubicacion suelo))
  (not (manosOcupadas (mono mono)))
  =>
  (retract (elegirAccion (accion mono-camina)))
  (printout t "El mono camina alrededor" crlf))

(defrule mono-mira-alrededor
  (elegirAccion (accion mono-mira-alrededor))
  ?mono <- (estaEn (objeto mono) (ubicacion mesa))
  (manosLibres (mono mono))
  =>
  (retract (elegirAccion (accion mono-mira-alrededor)))
  (printout t "El mono mira alrededor desde la mesa" crlf))

; Regla para imprimir el plan completo
(defrule imprimir-plan
  (Satisfecho (mono mono))
  =>
  (printout t "El mono ha completado su plan." crlf))

; Inicializar el sistema con los hechos
(reset)
; Afirmar hechos iniciales
(assert (estado-inicial))
; Ejecutar el sistema
(run)