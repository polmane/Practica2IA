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

(deffunction MAIN::ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction MAIN::yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then TRUE 
       else FALSE))

(deffunction MAIN::pregunta-numerica (?pregunta)
	(format t "%s" ?pregunta)
	(bind ?response (read))
	?response
)

;;***********************
;;** MODUL INFO USUARI **
;;***********************
(defrule info-usuari::pregunta-edat "Preguntem la edat de l usuari"
	?u <- (pregunta-numerica (tamano ?tamano) (nino ?n))
	(test (> ?tamano 1))
	(test (eq ?n NONE))
	=>
	(bind ?e (pregunta-si-no "Le acompanan menores de 12? "))
	(modify ?u (nino ?e))
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
;;; :Date 07/12/2022 02:43:46

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
    (multislot nivell_equilibri
        (type INTEGER)
        (create-accessor read-write))
    (multislot nivell_flexibilitat
        (type INTEGER)
        (create-accessor read-write))
    (multislot nivell_forca
        (type INTEGER)
        (create-accessor read-write))
    (multislot nivell_resistencia
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
    (multislot duracio
        (type INTEGER)
        (create-accessor read-write))
)

(defclass Solucio
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
)

(definstances instances
)
