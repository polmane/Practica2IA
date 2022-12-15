(deffunction pregunta (?pregunta $?valors-permesos) ;Si No
    (progn$
        (?var $?valors-permesos) ;; Si No s n
        (lowcase ?var)
        (printout t ?var)) ; Si No s n
    ;(printout t ( ?valors-permesos) )

    (printout t )
    (format t "%s? (%s) " ?pregunta (implode$ ?valors-permesos))
    (bind ?resposta (read))
    (while (not (member (lowcase ?resposta) ?valors-permesos)) do
        (format t "%s? (%s) " ?pregunta (implode$ ?valors-permesos))
        (bind ?resposta (read))
    )
    (printout t crlf)
    ?resposta
)

(deffunction pregunta-si-no (?pregunta)
    (bind ?resposta (pregunta ?pregunta Si No s n))
    (if (or (eq (lowcase ?resposta) si) (eq (lowcase ?resposta) s))
        then TRUE
        else FALSE
    )
)

(defrule pregunta-correr "Voldries correr"
    (not (pregunta-correr-feta))
	=>
    (bind ?e (pregunta-si-no "Voldries correr"))
    (assert (pregunta-correr-feta))
)