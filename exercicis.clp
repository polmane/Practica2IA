;;; ---------------------------------------------------------
;;; ontologia.clp
;;; Translated by owl2clips
;;; Translated to CLIPS from ontology ontologia.owl
;;; :Date 09/12/2022 00:07:39

(defclass Exercici
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (slot es_realitza
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
    (multislot pateix
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
    (multislot composta_per
        (type INSTANCE)
        (create-accessor read-write))
    (slot dia_solucio
        (type INTEGER)
        (create-accessor read-write))
    (slot temps_restant
        (type INTEGER)
        (create-accessor read-write))
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


; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxxxx

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
	(focus info-usuari)
)


;;;------------------------------------------------------------------------------------------------------------------------------------------------------
;;;----------  					TEMPLATES					 		---------- 								TEMPLATES
;;;------------------------------------------------------------------------------------------------------------------------------------------------------

;;; Template per les preguntes generals de l usuari
(deftemplate MAIN::pregunta-usuari
	(slot edat (type INTEGER))
	; (slot cor (type INSTANCE))
	; (slot fragilitat (type INSTANCE))
	; (slot hipertensio (type INSTANCE))
    ; (slot depressio (type INSTANCE))
    (multislot patologies (type INSTANCE))
)

;;; Template per el nivell fisic de la persona
(deftemplate MAIN::nivell-fisic
	(slot flexibilitat (type INTEGER) (default 0))
	(slot equilibri (type INTEGER) (default 0))
	(slot forca (type INTEGER) (default 0))
	(slot resistencia (type INTEGER) (default 0))
    (multislot recomanacions (type STRING))
)


;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction MAIN::pregunta (?pregunta $?valors-permesos)
    (progn$
        (?var ?valors-permesos)
        (lowcase ?var))
    (format t "%s? (%s) " ?pregunta (implode$ ?valors-permesos))
    (bind ?resposta (read))
    (while (not (member (lowcase ?resposta) ?valors-permesos)) do
        (format t "%s? (%s) " ?pregunta (implode$ ?valors-permesos))
        (bind ?resposta (read))
    )
    (printout t crlf)
    ?resposta
)


;;; Funcion para hacer una pregunta multi-respuesta con indices
(deffunction pregunta-multi (?pregunta $?valores-posibles)
    (bind ?linea (format nil "%s" ?pregunta))
    (printout t ?linea crlf)
    (progn$ (?var ?valores-posibles)
            (bind ?linea (format nil "  %d. %s" ?var-index ?var))
            (printout t ?linea crlf)
    )
    (format t "%s" "Indica els numero separats per un espai: ")
    (bind ?resp (readline))
    (bind ?numeros (str-explode ?resp))
    (bind $?lista (create$ ))
    (progn$ (?var ?numeros)
        (if (and (integerp ?var) (and (>= ?var 1) (<= ?var (length$ ?valores-posibles))))
            then
                (if (not (member$ ?var ?lista))
                    then (bind ?lista (insert$ ?lista (+ (length$ ?lista) 1) ?var))
                )
        )
    )
    (printout t crlf)
    ?lista
)


(deffunction MAIN::pregunta-si-no (?pregunta)
    (bind ?resposta (pregunta ?pregunta Si No s n))
    (if (or (eq (lowcase ?resposta) si) (eq (lowcase ?resposta) s))
        then TRUE
        else FALSE
    )
)


(deffunction MAIN::pregunta-si-no-depen (?pregunta)
    (bind ?resposta (pregunta ?pregunta Si No Depen s n d))
    (if (or (eq (lowcase ?resposta) si) (eq (lowcase ?resposta) s))
        then 1 ; Si
        else (if (or (eq (lowcase ?resposta) no) (eq (lowcase ?resposta) n))
            then -1 ; No
            else 0 ; Depen
        )
    )
)


(deffunction MAIN::pregunta-numerica (?pregunta ?rangini ?rangfi)
    (format t "%s? [%d, %d] " ?pregunta ?rangini ?rangfi)
    (bind ?resposta (read))
    (while (not(and(> ?resposta ?rangini)(< ?resposta ?rangfi))) do
        (format t "%s? [%d, %d] " ?pregunta ?rangini ?rangfi)
        (bind ?resposta (read))
    )
    (printout t crlf)
    ?resposta
)


;;***********************
;;** MODUL INFO USUARI **
;;***********************
(defrule info-usuari::pregunta-edat "Quina edat tens"
	(not (pregunta-usuari))
	=>
	(bind ?edat (pregunta-numerica "Quina edat tens" 65 150 ))
	(assert (pregunta-usuari (edat ?edat)))
)

(defrule info-usuari::pregunta-patologies "Quines patologies tens"
	?pref <- (pregunta-usuari)
    (not (pregunta-patologies-feta))
	=>
	(bind ?e (pregunta-si-no "Tens alguna patologia en concret"))
	(if (eq ?e TRUE)
	then (bind $?obj-patologies (find-all-instances ((?inst Patologia)) TRUE))
	(bind $?nom-patologies (create$ ))
	(loop-for-count (?i 1 (length$ $?obj-patologies)) do
		(bind ?curr-obj (nth$ ?i ?obj-patologies))
		(bind ?curr-nom (send ?curr-obj get-nom))
		(bind $?nom-patologies(insert$ $?nom-patologies (+ (length$ $?nom-patologies) 1) ?curr-nom))
	)
	(bind ?escogido (pregunta-multi "Escull les patologies que tens: " $?nom-patologies))

	(bind $?respuesta (create$ ))
	(loop-for-count (?i 1 (length$ ?escogido)) do
		(bind ?curr-index (nth$ ?i ?escogido))
		(bind ?curr-atr (nth$ ?curr-index ?obj-patologies))
		(bind $?respuesta(insert$ $?respuesta (+ (length$ $?respuesta) 1) ?curr-atr))
	)
	(modify ?pref (patologies $?respuesta))
	)
    (assert (pregunta-patologies-feta))
)

; (defrule info-usuari::pregunta-cor "Tens algun problema al cor"
; 	?u <- (pregunta-usuari (edat ?edat) (cor ?cor))
;     (test (> ?edat 0))
;     (test (eq ?cor [nil]))
; 	=>
; 	(bind ?e (pregunta-si-no "Tens algun problema al cor"))
; 	(modify ?u (cor ?e))
; )

;;; PREGUNTES RESISTENCIA
(defrule info-usuari::pregunta-escales "Et canses molt pujant escales"
    (not (nivell-fisic))
    (pregunta-patologies-feta)
    (not (pregunta-escales-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Et canses molt pujant escales"))
	(assert (nivell-fisic (resistencia (* ?e -1))))
    (assert (pregunta-escales-feta))
)

(defrule info-usuari::pregunta-caminar "Surts a caminar diariament"
    ?u <- (nivell-fisic (resistencia ?resistencia))
    (pregunta-escales-feta)
    (not (pregunta-caminar-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Surts a caminar diariament"))
	(modify ?u (resistencia (+ ?e ?resistencia)))
    (assert (pregunta-caminar-feta))
)

;;; PREGUNTES FLEXIBILITAT
(defrule info-usuari::pregunta-sabates "Et pots cordar les sabates sense ajuda"
    ?u <- (nivell-fisic)
    (pregunta-caminar-feta)
    (not (pregunta-sabates-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Et pots cordar les sabates sense ajuda"))
	(modify ?u (flexibilitat ?e))
    (assert (pregunta-sabates-feta))
)

(defrule info-usuari::pregunta-vestirte "Pots vestir-te sol/a"
    ?u <- (nivell-fisic (flexibilitat ?flexibilitat))
    (pregunta-sabates-feta)
    (not (pregunta-vestirte-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Pots vestir-te sol/a"))
	(modify ?u (flexibilitat (+ ?e ?flexibilitat)))
    (assert (pregunta-vestirte-feta))
)

;;; PREGUNTES FORCA
(defrule info-usuari::pregunta-cadira "Pots aixecar-te de la cadira"
    ?u <- (nivell-fisic)
    (pregunta-vestirte-feta)
    (not (pregunta-cadira-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Pots aixecar-te de la cadira"))
	(modify ?u (forca ?e))
    (assert (pregunta-cadira-feta))
)

(defrule info-usuari::pregunta-garrafa "Pots aixecar una garrafa de 8L"
    ?u <- (nivell-fisic (forca ?forca))
    (pregunta-cadira-feta)
    (not (pregunta-garrafa-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Pots aixecar una garrafa de 8L"))
	(modify ?u (forca (+ ?e ?forca)))
    (assert (pregunta-garrafa-feta))
)


;;; PREGUNTES EQUILIBRI
(defrule info-usuari::pregunta-suport "Utilitzes algun suport d equilibri per caminar"
    ?u <- (nivell-fisic)
    (pregunta-garrafa-feta)
    (not (pregunta-suport-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Utilitzes algun suport d equilibri per caminar"))
	(modify ?u (equilibri (* ?e -1)))
    (assert (pregunta-suport-feta))
)

(defrule info-usuari::pregunta-baixant "Et sents segur baixant escales"
    ?u <- (nivell-fisic (equilibri ?equilibri))
    (pregunta-suport-feta)
    (not (pregunta-baixant-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Et sents segur baixant escales"))
	(modify ?u (equilibri (+ ?e ?equilibri)))
    (assert (pregunta-baixant-feta))
)

; (defrule info-usuari::pregunta-fragilitat "Caus sovint"
; 	?u <- (pregunta-usuari (edat ?edat) (fragilitat ?fragilitat))
;     (test (eq ?fragilitat [nil]))
; 	=>
; 	(bind ?e (pregunta-si-no "Caus sovint"))
; 	(modify ?u (fragilitat ?e))
; )

; (defrule info-usuari::pregunta-hipertensio "Tens la pressio alta"
; 	?u <- (pregunta-usuari (edat ?edat) (hipertensio ?hipertensio))
;     (test (eq ?hipertensio [nil]))
; 	=>
; 	(bind ?e (pregunta-si-no "Tens la pressio alta"))
; 	(modify ?u (hipertensio ?e))
; )



; (defrule info-usuari::pregunta-fragilitat-2 "Recordes el que has fet aquest mati?"
; 	?u <- (pregunta-usuari (edat ?edat))
;     ?u <- (pregunta-usuari (fragilitat ?fragilitat))
; 	=>
; 	(bind ?e (or ?fragilitat  (pregunta-si-no "Recordes el que has fet aquest mati?")))
; 	(modify ?u (fragilitat ?e))
; )


; (defrule recomanar-exercici::exercici-caminar "Passejar/caminar"
;     ?u <- (pregunta-usuari (cor ?cor)
;     (test (eq ?cor TRUE))
;     =>
;     (assert (solucio-dia (edat ?edat)))
; )

