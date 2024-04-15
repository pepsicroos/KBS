(deftemplate enfermedades
    
    (multislot sintomas)
    (multislot signos)
    )

(defrule gripa
    (enfermedades  (signos fiebre cansancio) (sintomas dolor-de-garganta congestion-nasal))
    =>
    (assert (decision "Gripa")))

(defrule resfrio
    (enfermedades  (signos escalofrios estornudos) (sintomas dolor-de-cabeza congestion-nasal))
    =>
    (assert (decision "Resfrio")))

(defrule alergia
    (enfermedades  (signos picazon-en-los-ojos estornudos) (sintomas congestion-nasal erupcion-cutanea))
    =>
    (assert (decision "Alergia")))

(defrule migrana
    (enfermedades  (signos dolor-de-cabeza-intenso nauseas) (sintomas sensibilidad-a-la-luz sensibilidad-al-sonido))
    =>
    (assert (decision "Migrana")))

(defrule insomnio
    (enfermedades  (signos dificultad-para-dormir cansancio) (sintomas irritabilidad dificultad-de-concentracion))
    =>
    (assert (decision "Insomnio")))

(defrule diabetes
    (enfermedades (signos sed-excesiva perdida-de-peso) (sintomas fatiga vision-borrosa))
    =>
    (assert (decision "Diabetes")))



(defrule regla_default
    =>
    (assert (decision "No disponible")))

(defrule mostrar-decision
    (decision ?decision)
    =>
    (printout t "La decisión es: " ?decision crlf)
    (halt))