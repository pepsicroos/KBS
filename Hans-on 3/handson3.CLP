(deftemplate enfermedad
    (slot nombre)
    (multislot signos)
    (multislot sintomas)
)

(deffacts base-de-conocimientos
    (enfermedad (nombre gripe) (signos fiebre dolor-de-cabeza) (sintomas dolor-de-garganta congestion-nasal))
    (enfermedad (nombre resfriado) (signos escalofrios estornudos) (sintomas dolor-de-cabeza congestion-nasal))
    (enfermedad (nombre alergia) (signos picazon-en-los-ojos estornudos) (sintomas congestion-nasal erupcion-cutanea))
    (enfermedad (nombre migrana) (signos dolor-de-cabeza-intenso nauseas) (sintomas sensibilidad-a-la-luz sensibilidad-al-sonido))
    (enfermedad (nombre insomnio) (signos dificultad-para-dormir cansancio) (sintomas irritabilidad dificultad-de-concentracion))
    (enfermedad (nombre diabetes) (signos sed-excesiva perdida-de-peso) (sintomas fatiga vision-borrosa))
)

(deffunction hacer-pregunta (?atributo)
    (printout t "Tienes " ?atributo "? (SI/No): ")
)

(defrule inicio
    =>
    (printout t "Comenzando diagnostico..." crlf)
    (assert (enfermedades))
)

(defrule consultar-signo
    (enfermedades) (enfermedad (nombre ?nombre) (signos $?signos) (sintomas $?sintomas))
    (not (enfermedad-seleccionada (nombre ?nombre)))
    =>
    (bind ?signo (nth$ 1 ?signos)) ; Tomar el primer signo de la lista
    (hacer-pregunta ?signo)
    (assert (enfermedad-seleccionada (nombre ?nombre)))
)

(defrule respuesta-signo-si
    (input yes)
    (enfermedad-seleccionada (nombre ?nombre))
    (enfermedad (nombre ?nombre) (signos $?signos) (sintomas $?sintomas))
    =>
    (bind ?signo (nth$ 1 ?signos)) ; Tomar el primer signo de la lista
    (printout t "Puede ser " ?nombre " por el signo " ?signo "." crlf)
    (assert (diagnostico ?nombre))
)

(defrule respuesta-signo-no
    (input no)
    (enfermedad-seleccionada (nombre ?nombre))
    (enfermedad (nombre ?nombre) (signos $?signos) (sintomas $?sintomas))
    =>
    (bind ?signo (nth$ 1 ?signos)) ; Tomar el primer signo de la lista
    (printout t "No parece ser " ?nombre " por el signo " ?signo "." crlf)
)

(defrule consultar-sintoma
    (enfermedades)
    (enfermedad (nombre ?nombre) (signos $?signos) (sintomas $?sintomas))
    (not (enfermedad-seleccionada (nombre ?nombre)))
    =>
    (bind ?sintoma (nth$ 1 ?sintomas)) ; Tomar el primer sintoma de la lista
    (hacer-pregunta ?sintoma)
    (assert (enfermedad-seleccionada (nombre ?nombre)))
)

(defrule respuesta-sintoma-si
    (input yes)
    (enfermedad-seleccionada (nombre ?nombre))
    (enfermedad (nombre ?nombre) (signos $?signos) (sintomas $?sintomas))
    =>
    (bind ?sintoma (nth$ 1 ?sintomas)) ; Tomar el primer sintoma de la lista
    (printout t "Puede ser " ?nombre " por el sintoma " ?sintoma "." crlf)
    (assert (diagnostico ?nombre))
)

(defrule respuesta-sintoma-no
    (input no)
    (enfermedad-seleccionada (nombre ?nombre))
    (enfermedad (nombre ?nombre) (signos $?signos) (sintomas $?sintomas))
    =>
    (bind ?sintoma (nth$ 1 ?sintomas)) ; Tomar el primer sintoma de la lista
    (printout t "No parece ser " ?nombre " por el sintoma " ?sintoma "." crlf)
)

(defrule no-enfermedad-seleccionada
    (enfermedades)
    =>
    (printout t "Error: No se ha seleccionado ninguna enfermedad para evaluar." crlf)
    (halt)
)

(defrule diagnostico-final
    (diagnostico ?nombre)
    =>
    (printout t "El diagnostico es: " ?nombre "." crlf)
)

(defrule no-diagnostico-final
    (not (diagnostico ?))
    =>
    (printout t "No se pudo determinar un diagnostico." crlf)
)