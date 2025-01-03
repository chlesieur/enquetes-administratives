SELECT 		
GROUP_CONCAT(DISTINCT v.lib ORDER BY v.ordre) AS lib_voie,
e.id_etudiant,
e.id_nationalite,
e.bac_annee,
e.bac_serie,
e.id_ref_serie_bac,
e.bac_mention,
e.id_ref_bac_mention,
e.bac_lycee,
e.bac_ville,
e.bac_academie,
e.lib_voie_candidat,
e.libelle_statut_etudiant

FROM etudiant e 
LEFT JOIN voie_etudiant AS ve ON (ve.id_etudiant=e.id_etudiant)
LEFT JOIN voie AS v ON (ve.id_voie=v.id_voie)
LEFT JOIN nationalite nat ON (nat.id_nationalite=e.id_nationalite)

LEFT JOIN ( 
  SELECT 
    id_etudiant,
    SUM(montant_paiement) AS montant_paiement
  FROM frais_reglement
  WHERE temoin_ok=1
  GROUP BY id_etudiant
) aide_paiement ON (aide_paiement.id_etudiant=e.id_etudiant)

WHERE e.temoin_etudiant='1'
GROUP BY e.id_etudiant
ORDER BY 
  lib_voie, 
  e.nom, 
  e.prenom, 
  e.id_etudiant