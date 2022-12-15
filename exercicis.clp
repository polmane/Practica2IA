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

    ([10min] of Realitzacio
         (duracio  10)
    )

    ([15min] of Realitzacio
         (duracio  15)
    )

    ([25min] of Realitzacio
         (duracio  25)
    )

    ([40min] of Realitzacio
         (duracio  40)
    )

    ([6min] of Realitzacio
         (duracio  6)
    )

    ([8min] of Realitzacio
         (duracio  8)
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
    (multislot recomanacions (type STRING))
)

(deftemplate MAIN::nivell-fisic
    (slot flexibilitat (type STRING))
	(slot equilibri (type STRING))
	(slot forca (type STRING))
	(slot resistencia (type STRING))
    (slot total (type STRING))
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


;;; PREGUNTES RESISTENCIA
(defrule info-usuari::pregunta-escales "Et canses molt pujant escales"
    (not (punts-fisic))
    (pregunta-patologies-feta)
    (not (pregunta-escales-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Et canses molt pujant escales"))
	(assert (punts-fisic (resistencia (* ?e -1))))
    (assert (pregunta-escales-feta))
)

(defrule info-usuari::pregunta-caminar "Surts a caminar diariament"
    ?u <- (punts-fisic (resistencia ?resistencia))
    (pregunta-escales-feta)
    (not (pregunta-caminar-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Surts a caminar diariament"))
	(modify ?u (resistencia (+ ?e ?resistencia)))
    (assert (pregunta-caminar-feta))
)

;;; PREGUNTES FLEXIBILITAT
(defrule info-usuari::pregunta-sabates "Et pots cordar les sabates sense ajuda"
    ?u <- (punts-fisic)
    (pregunta-caminar-feta)
    (not (pregunta-sabates-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Et pots cordar les sabates sense ajuda"))
	(modify ?u (flexibilitat ?e))
    (assert (pregunta-sabates-feta))
)

(defrule info-usuari::pregunta-vestirte "Pots vestir-te sol/a"
    ?u <- (punts-fisic (flexibilitat ?flexibilitat))
    (pregunta-sabates-feta)
    (not (pregunta-vestirte-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Pots vestir-te sol/a"))
	(modify ?u (flexibilitat (+ ?e ?flexibilitat)))
    (assert (pregunta-vestirte-feta))
)

;;; PREGUNTES FORCA
(defrule info-usuari::pregunta-cadira "Pots aixecar-te de la cadira"
    ?u <- (punts-fisic)
    (pregunta-vestirte-feta)
    (not (pregunta-cadira-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Pots aixecar-te de la cadira"))
	(modify ?u (forca ?e))
    (assert (pregunta-cadira-feta))
)

(defrule info-usuari::pregunta-garrafa "Pots aixecar una garrafa de 8L"
    ?u <- (punts-fisic (forca ?forca))
    (pregunta-cadira-feta)
    (not (pregunta-garrafa-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Pots aixecar una garrafa de 8L"))
	(modify ?u (forca (+ ?e ?forca)))
    (assert (pregunta-garrafa-feta))
)


;;; PREGUNTES EQUILIBRI
(defrule info-usuari::pregunta-suport "Utilitzes algun suport d equilibri per caminar"
    ?u <- (punts-fisic)
    (pregunta-garrafa-feta)
    (not (pregunta-suport-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Utilitzes algun suport d equilibri per caminar"))
	(modify ?u (equilibri (* ?e -1)))
    (assert (pregunta-suport-feta))
)

(defrule info-usuari::pregunta-baixant "Et sents segur baixant escales"
    ?u <- (punts-fisic (equilibri ?equilibri))
    (pregunta-suport-feta)
    (not (pregunta-baixant-feta))
	=>
    (bind ?e (pregunta-si-no-depen "Et sents segur baixant escales"))
	(modify ?u (equilibri (+ ?e ?equilibri)))
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

(defrule analisi::abstraccio-nivell-problema "Abstraiem el problema"
    ?u <- (punts-fisic (equilibri ?equilibri) (flexibilitat ?flexibilitat) (forca ?forca) (resistencia ?resistencia))
    ?v <- (pregunta-usuari (patologies $?patologies))
    (not (nivell-fisic))
    =>

    (if (< ?equilibri 0)
        then (bind ?eq "baix")
        else
            (if (or (< ?equilibri 2) (member [Artritis] $?patologies) (member [Fragilitat] $?patologies) (member [Sobrepes] $?patologies))
                then (bind ?eq "moderat")
                else (bind ?eq "alt"))
    )

    (if (< ?flexibilitat 0)
        then (bind ?fl "baix")
        else
            (if (or (< ?flexibilitat 2) (member [Artritis] $?patologies) (member [Fragilitat] $?patologies) (member [Osteoporosi] $?patologies))
                then (bind ?fl "moderat")
                else (bind ?fl "alt"))
    )

    (if (< ?forca 0)
        then (bind ?fo "baix")
        else
            (if (or (< ?forca 2) (member [Artritis] $?patologies) (member [Fragilitat] $?patologies) (member [Osteoporosi] $?patologies))
                then (bind ?fo "moderat")
                else (bind ?fo "alt"))
    )

    (if (< ?resistencia 0)
        then (bind ?re "baix")
        else
            (if (or (< ?resistencia 2) (member [Cardiovascular] $?patologies) (member [Diabetis] $?patologies) (member [Hipertensio] $?patologies) (member [Malaltia_pulmonar] $?patologies) (member [Sobrepes] $?patologies))
                then (bind ?re "moderat")
                else (bind ?re "alt"))
    )
    (bind ?total_num (+ ?equilibri ?forca ?flexibilitat ?resistencia))

    (if (< ?total_num -2)
        then (bind ?total "baix")
        else
            (if (< ?total_num 4)
                then (bind ?total "moderat")
                else (bind ?total "alt"))
    )
    (assert (nivell-fisic (equilibri ?eq) (flexibilitat ?fl) (forca ?fo) (resistencia ?re) (total ?total)))
)

(defrule analisi::prioritat_tipus "Exercicis amb prioritat per culpa de la patologia"
     ?r <- (punts-fisic)
    (pregunta-usuari (patologies $?patologies))
    (not (prioritat_tipus_feta))
    =>
    
    (bind $?llista (create$))
    (if (or (member [Artritis] $?patologies) (member [Fragilitat] $?patologies) (member [Osteoporosi] $?patologies))
         then (bind $?llista (insert$ $?llista 1 "forca"))
             (bind $?llista (insert$ $?llista 1 "flexibilitat"))
     )

    (if (or (member [Artritis] $?patologies) (member [Fragilitat] $?patologies) (member [Sobrepes] $?patologies))
        then (bind $?llista (insert$ $?llista 1 "equilibri"))
    )

    (if (or (member [Cardiovascular] $?patologies) (member [Diabetis] $?patologies) (member [Hipertensio] $?patologies) (member [Malaltia_pulmonar] $?patologies) (member [Sobrepes] $?patologies))
        then (bind $?llista (insert$ $?llista 1 "resistencia"))
    )
    (modify ?r (recomanacions $?llista))
    (assert (prioritat_tipus_feta))
)

(defrule analisi::temps_solucio "Calculem quantitat de dies i minuts per dia "
    ?n <- (nivell-fisic (total ?total))
    ?e <- (pregunta-usuari (edat ?edat))
    (not (temporal))
    =>
    (bind ?m (+ (* -2 ?edat) 200))
    (bind ?d (div (- (* 0.1 ?edat) 3.5) 1))
    
    (if (> ?edat 85)
        then (bind ?m 30)
            (bind ?d 5)
        
    )

    (if (eq ?total "moderat")
        then (bind ?m (+ ?m 10))
                (bind ?d (+ ?d 1))
        
        else (if (eq ?total "alt")
            then (bind ?m (+ ?m 20))
                    (bind ?d (+ ?d 2))
        )
    )
    (assert (temporal (num_dies ?d) (minuts_dia ?m)))
)

(defrule analisi::asociacio_heuristica "asociacio heuristica"
    (nivell-fisic (equilibri ?equilibri) (flexibilitat ?flexibilitat) (forca ?forca) (resistencia ?resistencia))
    =>
    (bind $?obj-exs (find-all-instances ((?inst Resistencia)) TRUE))
    (loop-for-count (?i 1 (length$ $?obj-exs)) do
		(bind ?curr-obj (nth$ ?i ?obj-exs))
        (if (eq ?resistencia "moderat")
        then (bind ?m (+ ?m 10))
                (bind ?d (+ ?d 1))
        
        else (if (eq ?total "alt")
            then (bind ?m (+ ?m 20))
                    (bind ?d (+ ?d 2))
        )
    )
          (send ?curr-obj put-es_realitza 1)
	)
    ; (bind $?obj-exs (find-all-instances ((?inst Exercici)) TRUE))
    ; (loop-for-count (?i 1 (length$ $?obj-exs)) do
	; 	(bind ?curr-obj (nth$ ?i ?obj-exs))
    ;       (send ?curr-obj put-es_realitza 1)
	; )
    ; (bind $?obj-exs (find-all-instances ((?inst Exercici)) TRUE))
    ; (loop-for-count (?i 1 (length$ $?obj-exs)) do
	; 	(bind ?curr-obj (nth$ ?i ?obj-exs))
    ;       (send ?curr-obj put-es_realitza 1)
	; )
    ; (bind $?obj-exs (find-all-instances ((?inst Exercici)) TRUE))
    ; (loop-for-count (?i 1 (length$ $?obj-exs)) do
	; 	(bind ?curr-obj (nth$ ?i ?obj-exs))
    ;       (send ?curr-obj put-es_realitza 1)
	; )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defrule sintesi::relacio-exercicis "solucio abstracta"
    ?u <- (nivell-fisic (equilibri ?equilibri) (flexibilitat ?flexibilitat) (forca ?forca) (resistencia ?resistencia))
      ; cada ex dura 15 min
    =>
    (bind $?obj-exercicis (find-all-instances ((?inst Exercici)) TRUE))
    (bind $?nom-exercicis (create$ ))
	(loop-for-count (?i 1 (length$ $?obj-exercicis)) do
		(bind ?curr-obj (nth$ ?i ?obj-exercicis))
          (send ?curr-obj put-es_realitza 15)
	)
    ;; (focus sintesi)
)

(defrule sintesi::solRand "solucio aleatoria"
     (declare (salience 100))
     =>
     (bind ?mindia (+ (mod (random) 61) 30))
     (bind ?ndies (+ (mod (random) 5) 3))
     (bind ?exsdia (/ ?mindia 15))     ; cada ex dura 15 min
     (bind $?exs (find-all-instances ((?inst Exercici)) TRUE))

     (loop-for-count (?i 1 ?ndies) do
          (bind ?llista (create$))
          (bind ?sol (make-instance (sym-cat dia- (gensym)) of Solucio))
          (loop-for-count (?j 1 ?exsdia) do
               (bind ?rd (random-slot $?exs))
               (bind $?llista (insert$ $?llista (+ (length$ $?llista) 1) ?rd))
          )
          (send ?sol put-composta_per $?llista)
          (send ?sol put-dia_solucio ?i)
     )
     (focus imprimir)

)

	(deftemplate imprimir::min
		(slot count (type INTEGER))
		)

	(deffacts imprimir::hechos-iniciales
		(min (count 1))
	)

	(defrule imprimir::inicial "regla inicial"
		(declare (salience 10))
		=>
		(printout t crlf)
		(printout t "-----------------------------------------------------------------" crlf)
		(printout t "-------------------- Exercicis Personalitzats -------------------" crlf)
		(printout t "-----------------------------------------------------------------" crlf)
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
		(printout t crlf)
		(bind $?exercicis(send ?Dia get-composta_per))
		(loop-for-count (?i 1 (length$ $?exercicis)) do
				(bind ?curr-ex (nth$ ?i $?exercicis))
				(bind ?nom-exercicis (send ?curr-ex get-nom))
;;;				(printout t crlf)
				(format t "%s - 15 min" ?nom-exercicis)
				(printout t crlf)
		)
		(printout t crlf)
		(assert (min (count (+ ?n 1))))
		(retract ?min)
	)
