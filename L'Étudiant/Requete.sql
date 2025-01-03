SELECT
    e.etab_origine_ville,
    e.etab_origine_nom,
    e.etab_origine_formation,
    COUNT(*) AS nb_admis,
    e.id_ref_cpge
FROM
    etudiant e
WHERE
    e.concours_annee = '2024'
    AND e.id_ref_cpge <> 0 
GROUP BY
    e.etab_origine_ville, e.etab_origine_nom, e.etab_origine_formation;