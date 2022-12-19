;;; ---------------------------------------------------------
;;; ontologia.clp
;;; Translated by owl2clips
;;; Translated to CLIPS from ontology ontologia.owl
;;; :Date 18/12/2022 22:15:41

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
    ([Aixecament_lateral_cama] of Equilibri
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

    ([Correr] of Resistencia
         (nom  "Correr")
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

    ([Extensio_maluc] of Equilibri
         (nom  "Extensio de maluc")
    )

    ([Extensio_triceps] of Fortalesa
         (nom  "Extensio de triceps")
    )

    ([Felxio_plantar] of Equilibri
         (nom  "Flexio plantar")
    )

    ([Flexio_espatlla] of Fortalesa
         (nom  "Flexio d'espatlla")
    )

    ([Flexio_genoll] of Equilibri
         (nom  "Flexio de genoll")
    )

    ([Flexio_maluc] of Equilibri
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

    ([ResistenciaAlt] of Realitzacio
         (duracio  25)
    )

    ([ResistenciaBaix] of Realitzacio
         (duracio  15)
    )

    ([ResistenciaModerat] of Realitzacio
         (duracio  20)
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

    ([SeriesAlt] of Realitzacio
         (duracio  5)
    )

    ([SeriesBaix] of Realitzacio
         (duracio  3)
    )

    ([SeriesModerat] of Realitzacio
         (duracio  4)
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

;;; Modul d analisi
(defmodule analisi
	(import MAIN ?ALL)
	(export ?ALL)
)

(defmodule sintesi
	(import MAIN ?ALL)
	(export ?ALL)
)

(defmodule imprimir
	(import MAIN ?ALL)
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
    (slot correr (type SYMBOL))
    (slot nedar (type SYMBOL))
    (slot protesis-maluc (type SYMBOL))
    (multislot patologies (type STRING))
)

;;; Template per el nivell fisic de la persona
(deftemplate MAIN::punts-fisic
	(slot flexibilitat (type INTEGER) (default 0))
	(slot equilibri (type INTEGER) (default 0))
	(slot forca (type INTEGER) (default 0))
	(slot resistencia (type INTEGER) (default 0))
    (slot total (type INTEGER) (default 0))
)

(deftemplate MAIN::nivell-fisic
    (slot flexibilitat (type STRING))
	(slot equilibri (type STRING))
	(slot forca (type STRING))
	(slot resistencia (type STRING))
    (slot total (type STRING))
    (slot edat (type STRING))
    (multislot recomanacions (type STRING))
)

(deftemplate MAIN::temporal
    (slot num_dies (type INTEGER))
    (slot minuts_dia (type INTEGER))
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
    (bind ?resposta (pregunta ?pregunta si no s n))
    (if (or (eq (lowcase ?resposta) si) (eq (lowcase ?resposta) s))
        then TRUE
        else FALSE
    )
)


(deffunction MAIN::pregunta-si-no-depen (?pregunta)
    (bind ?resposta (pregunta ?pregunta si no depen s n d))
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
    (while (not(and(>= ?resposta ?rangini)(<= ?resposta ?rangfi))) do
        (format t "%s? [%d, %d] " ?pregunta ?rangini ?rangfi)
        (bind ?resposta (read))
    )
    (printout t crlf)
    ?resposta
)

(deffunction MAIN::random-slot ( ?li )
     (bind ?li (create$ ?li))
     (bind ?max (length ?li))
     (bind ?r (random 1 ?max))
     (bind ?ins (nth$ ?r ?li))
     (return ?ins)
)

(deffunction analisi::assignar-realitzacio (?tipus ?nivell)
    (bind $?obj-exs (find-all-instances ((?inst ?tipus)) TRUE))
    (loop-for-count (?i 1 (length$ $?obj-exs)) do
		(bind ?curr-obj (nth$ ?i ?obj-exs))
        (if (eq ?nivell "baix")
            then (if (eq ?tipus Resistencia)
                    then (send ?curr-obj put-es_realitza [ResistenciaBaix])
                    else (send ?curr-obj put-es_realitza [SeriesBaix])) 
            else (if (eq ?nivell "moderat")
                    then (
                        if (eq ?tipus Resistencia)
                            then (send ?curr-obj put-es_realitza [ResistenciaModerat])
                            else (send ?curr-obj put-es_realitza [SeriesModerat]))
                    else 
                        (if (eq ?tipus Resistencia)
                            then (send ?curr-obj put-es_realitza [ResistenciaAlt])
                            else (send ?curr-obj put-es_realitza [SeriesAlt]))

                )
        )
    )
)




;;***********************
;;** MODUL INFO USUARI **
;;***********************

(defrule info-usuari::pregunta-edat "Quina edat tens"
	(not (pregunta-usuari))
	=>
	(bind ?edat (pregunta-numerica "Quina edat tens" 65 150 ))
	(assert (pregunta-usuari (edat ?edat)))
    (seed ?edat)
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


;;; PREGUNTES RESISTENCIA
(defrule info-usuari::pregunta-escales "Et canses molt pujant escales"
    (not (punts-fisic))
    (pregunta-patologies-feta)
    (not (pregunta-escales-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Et canses molt pujant escales"))
	(assert (punts-fisic (resistencia (* ?e -1)) (total (* ?e -1))))
    (assert (pregunta-escales-feta))
)

(defrule info-usuari::pregunta-caminar "Surts a caminar diariament"
    ?u <- (punts-fisic (resistencia ?resistencia) (total ?total))
    (pregunta-escales-feta)
    (not (pregunta-caminar-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Surts a caminar diariament"))
	(modify ?u (resistencia (+ ?e ?resistencia)) (total (+ ?e ?total)))
    (assert (pregunta-caminar-feta))
)

;;; PREGUNTES FLEXIBILITAT
(defrule info-usuari::pregunta-sabates "Et pots cordar les sabates sense ajuda"
    ?u <- (punts-fisic (total ?total))
    (pregunta-caminar-feta)
    (not (pregunta-sabates-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Et pots cordar les sabates sense ajuda"))
	(modify ?u (flexibilitat ?e) (total (+ ?e ?total)))
    (assert (pregunta-sabates-feta))
)

(defrule info-usuari::pregunta-vestirte "Pots vestir-te sol/a"
    ?u <- (punts-fisic (flexibilitat ?flexibilitat) (total ?total))
    (pregunta-sabates-feta)
    (not (pregunta-vestirte-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Pots vestir-te sol/a"))
	(modify ?u (flexibilitat (+ ?e ?flexibilitat)) (total (+ ?e ?total)))
    (assert (pregunta-vestirte-feta))
)

;;; PREGUNTES FORCA
(defrule info-usuari::pregunta-cadira "Pots aixecar-te de la cadira"
    ?u <- (punts-fisic (total ?total))
    (pregunta-vestirte-feta)
    (not (pregunta-cadira-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Pots aixecar-te de la cadira"))
	(modify ?u (forca ?e) (total (+ ?e ?total)))
    (assert (pregunta-cadira-feta))
)

(defrule info-usuari::pregunta-garrafa "Pots aixecar una garrafa de 8L"
    ?u <- (punts-fisic (forca ?forca) (total ?total))
    (pregunta-cadira-feta)
    (not (pregunta-garrafa-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Pots aixecar una garrafa de 8L"))
	(modify ?u (forca (+ ?e ?forca)) (total (+ ?e ?total)))
    (assert (pregunta-garrafa-feta))
)


;;; PREGUNTES EQUILIBRI
(defrule info-usuari::pregunta-suport "Utilitzes algun suport d equilibri per caminar"
    ?u <- (punts-fisic (total ?total))
    (pregunta-garrafa-feta)
    (not (pregunta-suport-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Utilitzes algun suport d equilibri per caminar"))
	(modify ?u (equilibri (* ?e -1)) (total (+ (* ?e -1) ?total)))
    (assert (pregunta-suport-feta))
)

(defrule info-usuari::pregunta-baixant "Et sents segur baixant escales"
    ?u <- (punts-fisic (equilibri ?equilibri) (total ?total))
    (pregunta-suport-feta)
    (not (pregunta-baixant-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Et sents segur baixant escales"))
	(modify ?u (equilibri (+ ?e ?equilibri)) (total (+ ?e ?total)))
    (assert (pregunta-baixant-feta))
)

;;; PREGUNTES EXERCICIS CONCRETS
(defrule info-usuari::pregunta-correr "Voldries correr"
    ?u <- (pregunta-usuari)
    (pregunta-baixant-feta)
    (not (pregunta-correr-feta))
	=>
    (bind ?e (pregunta-si-no "Voldries correr"))
	(modify ?u (correr ?e))
    (assert (pregunta-correr-feta))
)

(defrule info-usuari::pregunta-nedar "Voldries nedar"
    ?u <- (pregunta-usuari)
    (pregunta-correr-feta)
    (not (pregunta-nedar-feta))
	=>
    (bind ?e (pregunta-si-no "Voldries nedar"))
	(modify ?u (nedar ?e))
    (assert (pregunta-nedar-feta))
)

(defrule info-usuari::pregunta-protesis-maluc "Tens protesi de maluc"
    ?u <- (pregunta-usuari)
    (pregunta-nedar-feta)
    (not (pregunta-protesis-maluc-feta))
	=>
    (bind ?e (pregunta-si-no "Tens protesi de maluc"))
	(modify ?u (protesis-maluc ?e))
    (assert (pregunta-protesis-maluc-feta))
    (focus analisi)
)


;;;;;;;;;;;;;;; EQUILIBRI ABSTRACCIO PROBLEMA
(defrule analisi::abstraccio-nivell-equilibri-baix "mirem si es nivell d equilibri baix"
    (punts-fisic (equilibri ?equilibri))
    (not (abstraccio-nivell-equilibri-feta))
    (test (< ?equilibri 0))

    =>
    
    (assert (abstraccio-nivell-equilibri-feta))
    (assert (nivell-fisic (equilibri "baix")))
)

(defrule analisi::abstraccio-nivell-equilibri-moderat "mirem si es nivell d equilibri moderat"
    (punts-fisic (equilibri ?equilibri))
    ?v <- (pregunta-usuari (patologies $?patologies))
    (not (abstraccio-nivell-equilibri-feta))
    (test (or 
        (and (< ?equilibri 2) (>= ?equilibri 0))
        (member [Artritis] $?patologies)
        (member [Fragilitat] $?patologies)
        (member [Sobrepes] $?patologies)
    ))

    =>

    (assert (abstraccio-nivell-equilibri-feta))
    (assert (nivell-fisic (equilibri "moderat")))
)

(defrule analisi::abstraccio-nivell-equilibri-alt "mirem si es nivell d equilibri alt"
    (punts-fisic (equilibri ?equilibri))
    ?v <- (pregunta-usuari (patologies $?patologies))
    (not (abstraccio-nivell-equilibri-feta))
    (test (and 
        (>= ?equilibri 2)
        (not(member [Artritis] $?patologies))
        (not(member [Fragilitat] $?patologies))
        (not(member [Sobrepes] $?patologies))
    ))

    =>

    (assert (abstraccio-nivell-equilibri-feta))
    (assert (nivell-fisic (equilibri "alt")))
)

;;;;;;;;;;;;;;; FLEXIBILITAT ABSTRACCIO PROBLEMA
(defrule analisi::abstraccio-nivell-flexibilitat-baix "mirem si es nivell de flexibilitat baix"
    (punts-fisic (flexibilitat ?flexibilitat))
    ?n <- (nivell-fisic)
    (abstraccio-nivell-equilibri-feta)
    (not(abstraccio-nivell-flexibilitat-feta))
    (test (< ?flexibilitat 0))

    =>
    
    (modify ?n (flexibilitat "baix"))
    (assert(abstraccio-nivell-flexibilitat-feta))
)

(defrule analisi::abstraccio-nivell-flexibilitat-moderat "mirem si es nivell de flexibilitat moderat"
    (punts-fisic (flexibilitat ?flexibilitat))
    ?n <- (nivell-fisic)
    ?v <- (pregunta-usuari (patologies $?patologies))
    (abstraccio-nivell-equilibri-feta)
    (not(abstraccio-nivell-flexibilitat-feta))
    (test (or 
        (and (< ?flexibilitat 2) (>= ?flexibilitat 0))
        (member [Artritis] $?patologies)
        (member [Fragilitat] $?patologies)
        (member [Osteoporosi] $?patologies)
    ))

    =>

    (modify ?n (flexibilitat "moderat"))
    (assert(abstraccio-nivell-flexibilitat-feta))
)

(defrule analisi::abstraccio-nivell-flexibilitat-alt "mirem si es nivell de flexibilitat alt"
    (punts-fisic (flexibilitat ?flexibilitat))
    ?n <- (nivell-fisic)
    ?v <- (pregunta-usuari (patologies $?patologies))
    (abstraccio-nivell-equilibri-feta)
    (not(abstraccio-nivell-flexibilitat-feta))
    (test (and 
        (>= ?flexibilitat 2)
        (not(member [Artritis] $?patologies))
        (not(member [Fragilitat] $?patologies))
        (not(member [Osteoporosi] $?patologies))
    ))

    =>

    (modify ?n (flexibilitat "alt"))
    (assert(abstraccio-nivell-flexibilitat-feta))
)

;;;;;;;;;;;;;;; FORCA ABSTRACCIO PROBLEMA
(defrule analisi::abstraccio-nivell-forca-baix "mirem si es nivell de forca baix"
    (punts-fisic (forca ?forca))
    ?n <- (nivell-fisic)
    (abstraccio-nivell-equilibri-feta)
    (not(abstraccio-nivell-forca-feta))
    (test (< ?forca 0))

    =>
    
    (modify ?n (forca "baix"))
    (assert(abstraccio-nivell-forca-feta))
)

(defrule analisi::abstraccio-nivell-forca-moderat "mirem si es nivell de forca moderat"
    (punts-fisic (forca ?forca))
    ?n <- (nivell-fisic)
    ?v <- (pregunta-usuari (patologies $?patologies))
    (abstraccio-nivell-equilibri-feta)
    (not(abstraccio-nivell-forca-feta))
    (test (or 
        (and (< ?forca 2) (>= ?forca 0))
        (member [Artritis] $?patologies)
        (member [Fragilitat] $?patologies)
        (member [Osteoporosi] $?patologies)
    ))

    =>

    (modify ?n (forca "moderat"))
    (assert(abstraccio-nivell-forca-feta))
)

(defrule analisi::abstraccio-nivell-forca-alt "mirem si es nivell de forca alt"
    (punts-fisic (forca ?forca))
    ?n <- (nivell-fisic)
    ?v <- (pregunta-usuari (patologies $?patologies))
    (abstraccio-nivell-equilibri-feta)
    (not(abstraccio-nivell-forca-feta))
    (test (and 
        (>= ?forca 2)
        (not(member [Artritis] $?patologies))
        (not(member [Fragilitat] $?patologies))
        (not(member [Osteoporosi] $?patologies))
    ))

    =>

    (modify ?n (forca "alt"))
    (assert(abstraccio-nivell-forca-feta))
)


;;;;;;;;;;;;;;; RESISTENCIA ABSTRACCIO PROBLEMA
(defrule analisi::abstraccio-nivell-resistencia-baix "mirem si es nivell de resistencia baix"
    (punts-fisic (resistencia ?resistencia))
    ?n <- (nivell-fisic)
    (abstraccio-nivell-equilibri-feta)
    (not(abstraccio-nivell-resistencia-feta))
    (test (< ?resistencia 0))

    =>
    
    (modify ?n (resistencia "baix"))
    (assert(abstraccio-nivell-resistencia-feta))
)

(defrule analisi::abstraccio-nivell-resistencia-moderat "mirem si es nivell de resistencia moderat"
    (punts-fisic (resistencia ?resistencia))
    ?n <- (nivell-fisic)
    ?v <- (pregunta-usuari (patologies $?patologies))
    (abstraccio-nivell-equilibri-feta)
    (not(abstraccio-nivell-resistencia-feta))
    (test (or 
        (and (< ?resistencia 2) (>= ?resistencia 0))
        (member [Cardiovascular] $?patologies)
        (member [Diabetis] $?patologies)
        (member [Hipertensio] $?patologies)
        (member [Malaltia_pulmonar] $?patologies)
        (member [Sobrepes] $?patologies)
    ))

    =>

    (modify ?n (resistencia "moderat"))
    (assert(abstraccio-nivell-resistencia-feta))
)

(defrule analisi::abstraccio-nivell-resistencia-alt "mirem si es nivell de resistencia alt"
    (punts-fisic (resistencia ?resistencia))
    ?n <- (nivell-fisic)
    ?v <- (pregunta-usuari (patologies $?patologies))
    (abstraccio-nivell-equilibri-feta)
    (not(abstraccio-nivell-resistencia-feta))
    (test (and 
        (>= ?resistencia 2)
        (not(member [Cardiovascular] $?patologies))
        (not(member [Diabetis] $?patologies))
        (not(member [Hipertensio] $?patologies))
        (not(member [Malaltia_pulmonar] $?patologies))
        (not(member [Sobrepes] $?patologies))
    ))

    =>

    (modify ?n (resistencia "alt"))
    (assert(abstraccio-nivell-resistencia-feta))
)

;;;;;;;;;;;;;;; FORMA TOTAL ABSTRACCIO PROBLEMA
(defrule analisi::abstraccio-nivell-total-baix "mirem si es nivell total es baix"
    (punts-fisic (total ?total))
    ?n <- (nivell-fisic)
    (not(abstraccio-nivell-total-feta))
    (test (< ?total -2))

    =>

    (modify ?n (total "baix"))
    (assert(abstraccio-nivell-total-feta))
)

(defrule analisi::abstraccio-nivell-total-moderat "mirem si es nivell total es moderat"
    (punts-fisic (total ?total))
    ?n <- (nivell-fisic)
    (not(abstraccio-nivell-total-feta))
    (test (and (>= ?total -2) (< ?total 4)))

    =>

    (modify ?n (total "moderat"))
    (assert(abstraccio-nivell-total-feta))
)

(defrule analisi::abstraccio-nivell-total-alt "mirem si es nivell total es alt"
    (punts-fisic (total ?total))
    ?n <- (nivell-fisic)
    (not(abstraccio-nivell-total-feta))
    (test (>= ?total 4))

    =>

    (modify ?n (total "alt"))
    (assert(abstraccio-nivell-total-feta))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; equilibri flexibilitat forca resistencia
(defrule analisi::prioritat_equilibri "Exercicis equilibri prioritat"
    (pregunta-usuari (patologies $?patologies))
    ?u <- (nivell-fisic (recomanacions $?recomanacions))
    (not(prioritat_equilibri_feta))
    (test (or (member [Artritis] $?patologies) (member [Fragilitat] $?patologies) (member [Sobrepes] $?patologies)))
    
    =>
    
    (bind $?recomanacions (insert$ $?recomanacions 1 "equilibri"))
    (modify ?u (recomanacions $?recomanacions))
    (assert(prioritat_equilibri_feta))
)

(defrule analisi::prioritat_flexibilitat "Exercicis flexibilitat prioritat"
    (pregunta-usuari (patologies $?patologies))
    ?u <- (nivell-fisic (recomanacions $?recomanacions))
    (not(prioritat_flexibilitat_feta))
    (test (or (member [Artritis] $?patologies) (member [Fragilitat] $?patologies) (member [Osteoporosi] $?patologies)))
    
    =>
    
    (bind $?recomanacions (insert$ $?recomanacions 1 "flexibilitat"))
    (modify ?u (recomanacions $?recomanacions))
    (assert(prioritat_flexibilitat_feta))
)

(defrule analisi::prioritat_forca "Exercicis forca prioritat"
    (pregunta-usuari (patologies $?patologies))
    ?u <- (nivell-fisic (recomanacions $?recomanacions))
    (not(prioritat_forca_feta))
    (test (or (member [Artritis] $?patologies) (member [Fragilitat] $?patologies) (member [Osteoporosi] $?patologies)))
    
    =>
    
    (bind $?recomanacions (insert$ $?recomanacions 1 "forca"))
    (modify ?u (recomanacions $?recomanacions))
    (assert(prioritat_forca_feta))
)

(defrule analisi::prioritat_resistencia "Exercicis resistencia prioritat"
    (pregunta-usuari (patologies $?patologies))
    ?u <- (nivell-fisic (recomanacions $?recomanacions))
    (not(prioritat_resistencia_feta))
    (test (or (member [Cardiovascular] $?patologies) (member [Diabetis] $?patologies) (member [Hipertensio] $?patologies) (member [Malaltia_pulmonar] $?patologies) (member [Sobrepes] $?patologies)))
    
    =>
    
    (bind $?recomanacions (insert$ $?recomanacions 1 "resistencia"))
    (modify ?u (recomanacions $?recomanacions))
    (assert(prioritat_resistencia_feta))
)

;;;;;;;;;;;;;;;;;ABSTRACCIO EDAT
(defrule analisi::abstraccio_anys_pocs "Abstraccio de pocs anys de la persona"
    (pregunta-usuari (edat ?edat))
    (not(abstraccio_anys_feta))
    ?n <- (nivell-fisic)
    (test (< ?edat 75))
    
    =>

    (modify ?n (edat "pocs"))
    (assert(abstraccio_anys_feta))
)

(defrule analisi::abstraccio_anys_bastants "Abstraccio de bastants anys de la persona"
    (pregunta-usuari (edat ?edat))
    (not(abstraccio_anys_feta))
    ?n <- (nivell-fisic)
    (test (and (>= ?edat 75)(< ?edat 85)))
    
    =>

    (modify ?n (edat "bastants"))
    (assert(abstraccio_anys_feta))
)

(defrule analisi::abstraccio_anys_molts "Abstraccio de molts anys de la persona"
    (pregunta-usuari (edat ?edat))
    (not(abstraccio_anys_feta))
    ?n <- (nivell-fisic)
    (test (>= ?edat 85))
    
    =>

    (modify ?n (edat "molts"))
    (assert(abstraccio_anys_feta))
)

(defrule analisi::iniciar_duracio "Posar al minim els dies i els minuts"
    (abstraccio_anys_feta)
    (abstraccio-nivell-total-feta)
    (not(iniciar_duracio_feta))

    =>

    (assert (temporal (num_dies 3) (minuts_dia 30)))
    (assert(iniciar_duracio_feta))
)

;;; Calcul dies i minuts
(defrule analisi::duracio_nivell_fisic_baix "Duracio nivell fisic baix"
    (nivell-fisic (total ?total))
    ?t <- (temporal (num_dies ?d) (minuts_dia ?m))
    (iniciar_duracio_feta)
    (not(duracio_nivell_fisic_feta))
    (test (eq ?total "baix"))

    =>

	(modify ?t (num_dies (+ 2 ?d)))
    (assert(duracio_nivell_fisic_feta))
)

(defrule analisi::duracio_nivell_fisic_moderat "Duracio nivell fisic moderat"
    (nivell-fisic (total ?total))
    ?t <- (temporal (num_dies ?d) (minuts_dia ?m))
    (iniciar_duracio_feta)
    (not(duracio_nivell_fisic_feta))
    (test (eq ?total "moderat"))

    =>

	(modify ?t (num_dies (+ 1 ?d)) (minuts_dia (+ 15 ?m)))
    (assert(duracio_nivell_fisic_feta))
)

(defrule analisi::duracio_nivell_fisic_alt "Duracio nivell fisic alt"
    (nivell-fisic (total ?total))
    ?t <- (temporal (num_dies ?d) (minuts_dia ?m))
    (iniciar_duracio_feta)
    (not(duracio_nivell_fisic_feta))
    (test (eq ?total "alt"))

    =>

    (modify ?t (minuts_dia (+ 30 ?m)))
    (assert(duracio_nivell_fisic_feta))
)

(defrule analisi::duracio_edat_molts "Duracio nivell fisic molts"
    (nivell-fisic (edat ?edat))
    ?t <- (temporal (num_dies ?d) (minuts_dia ?m))
    (iniciar_duracio_feta)
    (not(duracio_edat_feta))
    (test (eq ?edat "molts"))

    =>

	(modify ?t (num_dies (+ 2 ?d)))
    (assert(duracio_edat_feta))
)

(defrule analisi::duracio_edat_bastants "Duracio nivell fisic bastants"
    (nivell-fisic (edat ?edat))
    ?t <- (temporal (num_dies ?d) (minuts_dia ?m))
    (iniciar_duracio_feta)
    (not(duracio_edat_feta))
    (test (eq ?edat "bastants"))

    =>

	(modify ?t (num_dies (+ 1 ?d)) (minuts_dia (+ 15 ?m)))
    (assert(duracio_edat_feta))
)

(defrule analisi::duracio_edat_pocs "Duracio nivell fisic pocs"
    (nivell-fisic (edat ?edat))
    ?t <- (temporal (num_dies ?d) (minuts_dia ?m))
    (iniciar_duracio_feta)
    (not(duracio_edat_feta))
    (test (eq ?edat "pocs"))

    =>

    (modify ?t (minuts_dia (+ 30 ?m)))
    (assert(duracio_edat_feta))
)



;    n_total           baix moderada alta
; edat           (+0)(+2)  (+15)(+1)  (+30)(+0)
; pocs(+30)(+0)        5(60) 4(75)   3(90)     300 300 270     180 300 450
; bastants(+15)(+1)    6(45) 5(60)   4(75)     270 300 300     180 300 450
; molts(+0)(+2)        7(30) 6(45)   5(60)     210 270 300     150 270 420

; (defrule analisi::temps_solucio "Calculem quantitat de dies i minuts per dia"
;     ?n <- (nivell-fisic (total ?total))
;     ?e <- (pregunta-usuari (edat ?edat))
;     (not (temporal))
;     =>
;     (bind ?m (+ (* -2 ?edat) 200))
;     (bind ?d (div (- (* 0.1 ?edat) 3.5) 1))
    
;     (if (> ?edat 85)
;         then (bind ?m 30)
;             (bind ?d 5)
;     )

;     (if (eq ?total "moderat")
;         then (bind ?m (+ ?m 10))
;                 (bind ?d (+ ?d 1))
        
;         else (if (eq ?total "alt")
;             then (bind ?m (+ ?m 20))
;                     (bind ?d (+ ?d 2))
;         )
;     )
;     (assert (temporal (num_dies ?d) (minuts_dia ?m)))
; )

(defrule analisi::refinament_correr "treure Correr si usuari ho ha dit"
    (pregunta-usuari (correr ?correr&:(eq ?correr FALSE)))
    (not (refinament_correr_feta))

    =>

    (send [Correr] delete)
    (assert (refinament_correr_feta))
)

(defrule analisi::refinament_nedar "treure Natacio si usuari ho ha dit"
    (pregunta-usuari (nedar ?nedar&:(eq ?nedar FALSE)))
    (not (refinament_nedar_feta))

    =>

    (send [Natacio] delete)
    (assert (refinament_nedar_feta))
)

(defrule analisi::refinament_protesis_maluc "treure exercicis que no van be si tens una protesis maluc"
    (pregunta-usuari (protesis-maluc ?protesis_maluc&:(eq ?protesis_maluc TRUE)))
    (not (refinament_protesis_maluc_feta))

    =>

    (send [Rotacio_doble_maluc] delete)
    (send [Rotacio_simple_maluc] delete)
    (assert (refinament_protesis_maluc_feta))
)


;;;;;;;;;;;;;;;;;;;;;

(defrule analisi::refinament_realitzacio "asociacio heuristica"
    (declare (salience -100))
    (nivell-fisic (equilibri ?equilibri) (flexibilitat ?flexibilitat) (forca ?forca) (resistencia ?resistencia))
    =>
    (assignar-realitzacio Resistencia ?resistencia)
    (assignar-realitzacio Equilibri ?equilibri)
    (assignar-realitzacio Flexibilitat ?flexibilitat)
    (assignar-realitzacio Fortalesa ?forca)
    (focus sintesi)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftemplate sintesi::dia-actual
    (slot dia (type INSTANCE))
)

(deftemplate sintesi::num-dia-actual
    (slot count_dies (type INTEGER))
)

(deftemplate sintesi::exercicis-pendents
    (multislot flexibilitat (type INSTANCE))
    (multislot forca (type INSTANCE))
    (multislot resistencia (type INSTANCE))
    (multislot equilibri (type INSTANCE))
)

(deffacts sintesi::iniciar-sintesi
    (num-dia-actual (count_dies 1))
    (exercicis-pendents)
)

;;INI FLEXIBILITAT
(defrule sintesi::iniciar-exercicis-flexibilitat-pendents "posar tot el conjunt"
    (declare (salience 10))
    ?e <- (exercicis-pendents (flexibilitat $?ef)) 
    (test (<= (length$ $?ef) 0))

    =>

    (bind $?llista (find-all-instances ((?inst Flexibilitat)) TRUE))
    (modify ?e (flexibilitat $?llista))
)

;;INI FORCA
(defrule sintesi::iniciar-exercicis-forca-pendents "posar tot el conjunt"
    (declare (salience 10))
    ?e <- (exercicis-pendents (forca $?ef)) 
    (test (<= (length$ $?ef) 0))

    =>

    (bind $?llista (find-all-instances ((?inst Fortalesa)) TRUE))
    (modify ?e (forca $?llista))
)

;;INI EQUILIBRI
(defrule sintesi::iniciar-exercicis-equilibri-pendents "posar tot el conjunt"
    (declare (salience 10))
    ?e <- (exercicis-pendents (equilibri $?ef)) 
    (test (<= (length$ $?ef) 0))

    =>

    (bind $?llista (find-all-instances ((?inst Equilibri)) TRUE))
    (modify ?e (equilibri $?llista))
)

;;INI RESISTENCIA
(defrule sintesi::iniciar-exercicis-resistencia-pendents "posar tot el conjunt"
    (declare (salience 10))
    ?e <- (exercicis-pendents (resistencia $?ef)) 
    (test (<= (length$ $?ef) 0))

    =>

    (bind $?llista (find-all-instances ((?inst Resistencia)) TRUE))
    (modify ?e (resistencia $?llista))
)

;; INICIAR UN NOU DIA
(defrule sintesi::obrir-nou-dia "Obrir un dia on anirem afegint realitzacions i exercicis"
    (not (dia-a-mig-omplir))
    (num-dia-actual (count_dies ?cd))
    (temporal (num_dies ?nd) (minuts_dia ?md))
	(test (<= ?cd ?nd))

    =>

    (printout t "obrir dia" crlf)
    (bind ?diaNou (make-instance (sym-cat dia- (gensym)) of Solucio))
    (send ?diaNou put-dia_solucio ?cd)
    (send ?diaNou put-temps_restant ?md)
    (assert (dia-actual (dia ?diaNou)))
    (assert (dia-a-mig-omplir))
)

(defrule sintesi::afegir-exercicis-escalfament "Afegir exercici escalfament"
	; (declare (salience 1000))
    ?e <- (exercicis-pendents (flexibilitat $?ef))
    (temporal (minuts_dia ?md))
    (dia-actual (dia ?inst))
    (test (> (send ?inst get-temps_restant) (* ?md (/ 9 10))))
    (test (> (length$ $?ef) 0))

    =>
    
    (printout t "afegir" crlf)
    (while (> (send ?inst get-temps_restant) (* ?md (/ 90 100))) do
        (bind $?ll (send ?inst get-composta_per))
        (bind ?ex_escollit (random-slot ?ef))
        (send ?inst put-composta_per (insert$ ?ll (+ (length$ ?ll) 1)  ?ex_escollit))

        (bind ?tr (send ?inst get-temps_restant))
        (bind ?realitzacio (send ?ex_escollit get-es_realitza))
        (send ?inst put-temps_restant (- ?tr (send ?realitzacio get-duracio)))
    )
)

(deffunction sintesi::exercici-cap-al-dia (?exercici ?dia)
    (bind ?realitzacio (send ?exercici get-es_realitza))
    (bind ?tr (send ?dia get-temps_restant))
    (if (>= ?tr (send ?realitzacio get-duracio))
        then TRUE
        else FALSE
    )
)

;; TANCAR DIA COM A COMPLERT
(defrule sintesi::tancar-dia-ple "Donar un dia per tancat quan ja estan planificats totes les activitats que hi caben"
    ?d <- (dia-a-mig-omplir)
    ?nda <- (num-dia-actual (count_dies ?cd))
    ?da <- (dia-actual (dia ?dia))
    ?ep <- (exercicis-pendents (flexibilitat $?efl) (forca $?efo) (equilibri $?eeq) (resistencia $?ere))
    (test (not (any-instancep ((?inst Exercici)) (and (exercici-cap-al-dia ?inst ?dia) (or (member ?inst $?efl) (member ?inst $?efo) (member ?inst $?eeq) (member ?inst $?ere))))))

    =>
    
    (printout t "tancar dia" crlf)
    (modify ?nda (count_dies (+ ?cd 1)))
    (retract ?da)
    (retract ?d)
)






; (defclass Solucio
;     (is-a USER)
;     (role concrete)
;     (pattern-match reactive)
;     (multislot composta_per
;         (type INSTANCE)
;         (create-accessor read-write))
;     (slot dia_solucio
;         (type INTEGER)
;         (create-accessor read-write))
;     (slot temps_restant
;         (type INTEGER)
;         (create-accessor read-write))
; )

; (defrule sintesi::relacio-exercicis "solucio abstracta"
;     ?u <- (nivell-fisic (equilibri ?equilibri) (flexibilitat ?flexibilitat) (forca ?forca) (resistencia ?resistencia))
;       ; cada ex dura 15 min
;     =>
;     (bind $?obj-exercicis (find-all-instances ((?inst Exercici)) TRUE))
;     (bind $?nom-exercicis (create$ ))
; 	(loop-for-count (?i 1 (length$ $?obj-exercicis)) do
; 		(bind ?curr-obj (nth$ ?i ?obj-exercicis))
;           (send ?curr-obj put-es_realitza 15)
; 	)
;     ;; (focus sintesi)
; )



; (defrule sintesi::solRand "solucio aleatoria"
;      (declare (salience 100))
;      =>
;      (bind ?mindia (+ (mod (random) 61) 30))
;      (bind ?ndies (+ (mod (random) 5) 3))
;      (bind ?exsdia (/ ?mindia 15))     ; cada ex dura 15 min
;      (bind $?exs (find-all-instances ((?inst Exercici)) TRUE))

;      (loop-for-count (?i 1 ?ndies) do
;           (bind ?llista (create$))
;           (bind ?sol (make-instance (sym-cat dia- (gensym)) of Solucio))
;           (loop-for-count (?j 1 ?exsdia) do
;                (bind ?rd (random-slot $?exs))
;                (bind $?llista (insert$ $?llista (+ (length$ $?llista) 1) ?rd))
;           )
;           (send ?sol put-composta_per $?llista)
;           (send ?sol put-dia_solucio ?i)
;      )
;      (focus imprimir)

; )

(deftemplate imprimir::min
    (slot count (type INTEGER))
)

(deffacts imprimir::hechos-iniciales
    (min (count 1))
)

(defrule imprimir::inicial "regla inicial"
    (declare (salience 10))
    (temporal (num_dies ?d) (minuts_dia ?m))
    =>
    (printout t crlf)
    (printout t "-----------------------------------------------------------------" crlf)
    (printout t "-------------------- Exercicis Personalitzats -------------------" crlf)
    (format t "--------------------(%d dies, aprox. %d minuts)-------------------" ?d ?m)
    (printout t crlf)
    (printout t "-----------------------------------------------------------------" crlf)
    (printout t crlf)
)


(defrule imprimir::imprimir-dia
    ?min <- (min (count ?n))
    ?Dia <- (object (is-a Solucio) (dia_solucio ?numDia))
    (test (eq ?n ?numDia))
    =>
    (format t "Els exercicis a fer el dia %d son:" ?n)
    (printout t crlf)
    (printout t "-------------------------------------------------------------" crlf)
    ;(printout t crlf)
    (bind $?exercicis(send ?Dia get-composta_per))
    (loop-for-count (?i 1 (length$ $?exercicis)) do
            (bind ?curr-ex (nth$ ?i $?exercicis))
            (bind ?nom-exercici (send ?curr-ex get-nom))
            (bind ?realitzacio_ex (send ?curr-ex get-es_realitza))
;;;				(printout t crlf)
            (format t "%s - %d min" ?nom-exercici (send ?realitzacio_ex get-duracio))
            (printout t crlf)
    )
    (printout t crlf)
    (assert (min (count (+ ?n 1))))
    (retract ?min)
)
