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
