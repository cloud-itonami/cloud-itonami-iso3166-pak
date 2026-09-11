(ns marketentry.facts "Pakistan market-entry catalog.")
(def catalog
  {"PAK" {:name "Pakistan"
          :owner-authority "PPRA / e-PADS / EPADS"
          :legal-basis "Public Procurement Regulatory Authority Ordinance"
          :national-spec "e-PADS supplier registration + SECP/NTN"
          :provenance "https://www.ppra.org.pk/"
          :required-evidence ["SECP/NTN record" "e-PADS registration record" "SECP extract" "Authorized-representative record"]
          :rep-owner-authority "contracting authorities / PPRA"
          :rep-legal-basis "Pakistani legal entity (SECP/NTN) typically required for federal awards"
          :rep-provenance "https://www.ppra.org.pk/"
          :corporate-number-owner-authority "SECP / FBR"
          :corporate-number-legal-basis "Incorporation number / NTN"
          :corporate-number-provenance "https://www.secp.gov.pk/"}
   "USA" {:name "United States" :owner-authority "GSA/SAM.gov" :legal-basis "FAR" :national-spec "SAM.gov" :provenance "https://sam.gov/"
          :required-evidence ["EIN record" "SAM.gov registration record" "State business registration record" "SAM UEI verification record"]}
   "IND" {:name "India" :owner-authority "GeM" :legal-basis "GFR" :national-spec "GeM" :provenance "https://gem.gov.in/"
          :required-evidence ["GSTIN/PAN record" "GeM seller registration" "MCA extract" "Authorized-representative record"]}
   "CHN" {:name "China" :owner-authority "CCGP" :legal-basis "GPL" :national-spec "CCGP" :provenance "http://www.ccgp.gov.cn/"
          :required-evidence ["USCC record" "CCGP registration" "Business license" "Authorized-representative record"]}})

(defn spec-basis [iso3] (get catalog iso3))
(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s) missing (remove catalog iso3s)]
     {:requested (count iso3s) :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note "R0 catalog seed"})))
(defn required-evidence-satisfied? [iso3 submitted]
  (when-let [{:keys [required-evidence]} (spec-basis iso3)]
    (= (count required-evidence) (count (filter (set submitted) required-evidence)))))
(defn evidence-checklist [iso3] (:required-evidence (spec-basis iso3) []))
(defn rep-spec-basis [iso3]
  (when-let [sb (spec-basis iso3)]
    (when (:rep-owner-authority sb)
      (select-keys sb [:rep-owner-authority :rep-legal-basis :rep-provenance]))))
(defn corporate-number-spec-basis [iso3]
  (when-let [sb (spec-basis iso3)]
    (when (:corporate-number-owner-authority sb)
      (select-keys sb [:corporate-number-owner-authority :corporate-number-legal-basis :corporate-number-provenance]))))
