;;;======================================================
;;;   Sistema Expert d'Exercicis Saludables Personalitzats
;;;
;;;     Aquest sistema expert recomana exercicis per a
;;;     persones grans.
;;;
;;;     CLIPS Version 6.3
;;;
;;;     Load, reset and run.
;;;======================================================

;;;------------------------------------------------------------------------------------------------------------------------------------------------------
;;;----------  					 MAIN					 		---------- 								MAIN
;;;------------------------------------------------------------------------------------------------------------------------------------------------------

;; Aquest es el modul principal

(defmodule MAIN (export ?ALL))

;;; Modul de recopilació
(defmodule info-usuari
	(import MAIN ?ALL)
	(export ?ALL)
)

(defrule MAIN::initialRule "regla inicial"
	(declare (salience 10))
	=>
	(printout t crlf)
	(printout t "--------------------------------------------------------------" crlf)
	(printout t "--------- Recomanacio d'exercicis per a persones grans -------" crlf)
	(printout t "--------------------------------------------------------------" crlf)
	(printout t crlf)
	(focus recopilacio-usuari)
)


;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction MAIN::pregunta (?pregunta $?valors-permesos)
    (progn$
        (?var ?valors-permesos)
        (lowcase ?var))
    (format t "¿%s? (%s) " ?pregunta (implode$ ?valors-permesos))
    (bind ?resposta (read))
    (while (not (member (lowcase ?resposta) ?valors-permesos)) do
        (format t "¿%s? (%s) " ?pregunta (implode$ ?valors-permesos))
        (bind ?resposta (read))
    )
    ?resposta
)


(deffunction MAIN::si-o-no-p (?pregunta)
    (bind ?resposta (pregunta ?pregunta si no s n))
    (if (or (eq (lowcase ?resposta) si) (eq (lowcase ?resposta) s))
        then TRUE
        else FALSE
    )
)


(deffunction MAIN::pregunta-numerica (?pregunta ?rangini ?rangfi)
    (format t "¿%s? [%d, %d] " ?pregunta ?rangini ?rangfi)
    (bind ?resposta (read))
    (while (not(and(> ?resposta ?rangini)(< ?resposta ?rangfi))) do
        (format t "¿%s? [%d, %d] " ?pregunta ?rangini ?rangfi)
        (bind ?resposta (read))
    )
    ?resposta
)


;;***********************
;;** MODUL INFO USUARI **
;;***********************
(defrule info-usuari::pregunta-edat "Quina edat tens?"
	(not (pregunta-usuari))
	=>
	(bind ?edat (pregunta-numerica "Quina edat tens? " 65 150 ))
	(assert (pregunta-usuari (edat ?edat)))
)

(defrule info-usuari::pregunta-cor "Tens algun problema al cor?"
	?u <- (pregunta-usuari (edat ?edat))
	=>
	(bind ?e (pregunta-si-no "Tens algun problema al cor?"))
	(modify ?u (cor ?e))
)


;;;------------------------------------------------------------------------------------------------------------------------------------------------------
;;;----------  					TEMPLATES					 		---------- 								TEMPLATES
;;;------------------------------------------------------------------------------------------------------------------------------------------------------

;;; Template per les preguntes generals de l usuari
(deftemplate MAIN::pregunta-usuari
	(slot edat (type INTEGER))
	(slot cor (type INSTANCE))
	(slot fragilitat (type INSTANCE))
	(slot hipertensio (type INSTANCE))
    (slot depressio (type INSTANCE))
)

;;; Template per el nivell fisic de la persona
(deftemplate MAIN::nivell-fisic
	(slot flexibilitat (type STRING))
	(slot equilibri (type STRING))
	(slot forca (type STRING))
	(slot resistencia (type STRING))
)




;;; ---------------------------------------------------------
;;; ontologia.clp
;;; Translated by owl2clips
;;; Translated to CLIPS from ontology ontologia.owl
;;; :Date 08/12/2022 14:44:37

(defclass Exercici
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot es_realitza
        (type INSTANCE)
        (create-accessor read-write))
    (slot nom
        (type STRING)
        (create-accessor read-write))
)

(defclass Equilibri
    (is-a Exercici)
    (role concrete)
    (pattern-match reactive)
)

(defclass Flexibilitat
    (is-a Exercici)
    (role concrete)
    (pattern-match reactive)
)

(defclass Fortalesa
    (is-a Exercici)
    (role concrete)
    (pattern-match reactive)
)

(defclass FortalesaEquilibri
    (is-a Exercici)
    (role concrete)
    (pattern-match reactive)
)

(defclass Resistencia
    (is-a Exercici)
    (role concrete)
    (pattern-match reactive)
)

(defclass Patologia
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (slot nom
        (type STRING)
        (create-accessor read-write))
)

(defclass Persona
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (slot pateix_una
        (type INSTANCE)
        (create-accessor read-write))
    (slot edat
        (type INTEGER)
        (create-accessor read-write))
    (slot nivell_equilibri
        (type INTEGER)
        (create-accessor read-write))
    (slot nivell_flexibilitat
        (type INTEGER)
        (create-accessor read-write))
    (slot nivell_forca
        (type INTEGER)
        (create-accessor read-write))
    (slot nivell_resistencia
        (type INTEGER)
        (create-accessor read-write))
    (slot nom
        (type STRING)
        (create-accessor read-write))
)

(defclass Realitzacio
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (slot duracio
        (type INTEGER)
        (create-accessor read-write))
    (multislot intensitat
        (type STRING)
        (create-accessor read-write))
)

(defclass Solucio
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
)

(definstances instances
    ([Aixecament_lateral_cama] of FortalesaEquilibri
         (nom  "Aixecament lateral de cames")
    )

    ([Aixecar-se_cadira] of Fortalesa
         (nom  "Aixecar-se d'una cadira")
    )

    ([Aixecar_bracos] of Fortalesa
         (nom  "Aixecar bracos lateralment")
    )

    ([Artritis] of Patologia
         (nom  "Artritis")
    )

    ([Caminar] of Resistencia
         (nom  "Caminar")
    )

    ([Cancer] of Patologia
         (nom  "Cancer")
    )

    ([Cardiovascular] of Patologia
         (nom  "Cardiovascular")
    )

    ([Dansa] of Resistencia
         (nom  "Dansa")
    )

    ([Depressio] of Patologia
         (nom  "Depressio")
    )

    ([Diabetis] of Patologia
         (nom  "Diabetis")
    )

    ([Estirament_bessons] of Flexibilitat
         (nom  "Estirament de bessons")
    )

    ([Estirament_canell] of Flexibilitat
         (nom  "Estirament de canell")
    )

    ([Estirament_quadriceps] of Flexibilitat
         (nom  "Estirament de quadriceps")
    )

    ([Estirament_triceps] of Flexibilitat
         (nom  "Estirament de triceps")
    )

    ([Estirament_turmell] of Flexibilitat
         (nom  "Estirament de turmell")
    )

    ([Exercicis_biceps] of Fortalesa
         (nom  "Exercicis de biceps")
    )

    ([Exercicis_equilibri] of Equilibri
         (nom  "Exercicis d'equilibri")
    )

    ([Extensio_genoll] of Fortalesa
         (nom  "Extensio de genoll")
    )

    ([Extensio_maluc] of FortalesaEquilibri
         (nom  "Extensio de maluc")
    )

    ([Extensio_triceps] of Fortalesa
         (nom  "Extensio de triceps")
    )

    ([Felxio_plantar] of FortalesaEquilibri
         (nom  "Flexio plantar")
    )

    ([Flexio_espatlla] of Fortalesa
         (nom  "Flexio d'espatlla")
    )

    ([Flexio_genoll] of FortalesaEquilibri
         (nom  "Flexio de genoll")
    )

    ([Flexio_maluc] of FortalesaEquilibri
         (nom  "Flexio de maluc")
    )

    ([Fragilitat] of Patologia
         (nom  "Fragilitat")
    )

    ([Hipertensio] of Patologia
         (nom  "Hipertensio")
    )

    ([Malaltia_pulmonar] of Patologia
         (nom  "Malaltia pulmonar")
    )

    ([Natacio] of Resistencia
         (nom  "Natacio")
    )

    ([Osteoporosi] of Patologia
         (nom  "Osteoporosi")
    )

    ([Pedalar] of Resistencia
         (nom  "Pedalar")
    )

    ([Pujar_escales] of Resistencia
         (nom  "Pujar escales")
    )

    ([Remar] of Resistencia
         (nom  "Remar")
    )

    ([Rotacio_doble_maluc] of Flexibilitat
         (nom  "Rotacio doble de maluc")
    )

    ([Rotacio_espatlla] of Flexibilitat
         (nom  "Rotacio d'espatlla")
    )

    ([Rotacio_simple_maluc] of Flexibilitat
         (nom  "Rotacio simple de maluc")
    )

    ([Sobrepes] of Patologia
         (nom  "Sobrepes")
    )

)
