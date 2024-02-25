(deftemplate enfermedad (slot nombre) (multislot signos) (multislot sintomas))


(deffacts base-de-conocimientos
    (enfermedad (nombre gripe) (signos fiebre cansancio) (sintomas dolor-de-garganta congestion-nasal))
    (enfermedad (nombre resfrio) (signos escalofrios estornudos) (sintomas dolor-de-cabeza congestion-nasal))
    (enfermedad (nombre alergia) (signos picazon-en-los-ojos estornudos) (sintomas congestion-nasal erupcion-cutanea))
    (enfermedad (nombre migrana) (signos dolor-de-cabeza-intenso nauseas) (sintomas sensibilidad-a-la-luz sensibilidad-al-sonido))
    (enfermedad (nombre insomnio) (signos dificultad-para-dormir cansancio) (sintomas irritabilidad dificultad-de-concentracion))
    (enfermedad (nombre diabetes) (signos sed-excesiva perdida-de-peso) (sintomas fatiga vision-borrosa))
)



(defrule consultar-enfermedad
    ?f <- (enfermedad (nombre ?nombre) (signos $?signos) (sintomas $?sintomas))
    =>
    (printout t "Enfermedad: " ?nombre crlf)
    (printout t "Signos: " ?signos crlf)
    (printout t "Síntomas: " ?sintomas crlf)
)

(defrule eliminar-enfermedad
    ?f <- (enfermedad (nombre ?nombre) (signos $?signos) (sintomas $?sintomas))
    =>
    (retract ?f)
    (printout t "Enfermedad eliminada: " ?nombre crlf)
)
