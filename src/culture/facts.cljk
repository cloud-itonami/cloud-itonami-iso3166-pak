(ns culture.facts
  "Country-level regional-culture catalog for Pakistan (PAK) -- national
  dishes, protected products, beverages, crafts, festivals and heritage
  sites, per ADR-2607171400 addendum 2 (cloud-itonami-municipality-
  culture-catalog Wave 1, in com-junkawasaki/root). Sibling namespace to
  `marketentry.facts` / `statute.facts` (ADR-2607141700); city-level
  counterparts live in the cloud-itonami-municipality-* repos.

  Catalog is keyed by UPPERCASE ISO3 (mirrors `statute.facts`); entries
  carry no :culture/municipality (that attribute is city-level only).

  Every entry cites a source URL that was actually fetched and read on
  :culture/retrieved-at -- never fabricated. Summaries state only what the
  cited source confirms. An item not in this table has NO spec-basis, full
  stop; extend `catalog`, do not invent an id/url.")

(def catalog
  "iso3 -> vector of culture entries."
  {"PAK"
   [{:culture/id "pak.dish.sindhi-biryani"
     :culture/name "Sindhi Biryani"
     :culture/country "PAK"
     :culture/kind :dish
     :culture/summary "Meat-and-rice biryani dish originating from the Sindh province of Pakistan."
     :culture/url "https://en.wikipedia.org/wiki/Sindhi_biryani"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "pak.dish.chapli-kebab"
     :culture/name "Chapli Kebab"
     :culture/country "PAK"
     :culture/kind :dish
     :culture/summary "Minced-meat kebab that originated in the city of Peshawar, Khyber Pakhtunkhwa province, Pakistan, and is today found across South Asia."
     :culture/url "https://en.wikipedia.org/wiki/Chapli_kebab"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "pak.dish.nihari"
     :culture/name "Nihari"
     :culture/country "PAK"
     :culture/kind :dish
     :culture/summary "Slow-cooked stew that originated on the Indian subcontinent and became a large-scale success in Karachi after 1947, spreading in prominence and availability across Pakistan."
     :culture/url "https://en.wikipedia.org/wiki/Nihari"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "pak.beverage.lassi"
     :culture/name "Lassi"
     :culture/country "PAK"
     :culture/kind :beverage
     :culture/summary "Yogurt-based drink associated with Punjabi, Indian and Pakistani cuisine, part of the broader South Asian culinary tradition rather than uniquely Pakistani."
     :culture/url "https://en.wikipedia.org/wiki/Lassi"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "pak.craft.kheri"
     :culture/name "Kheri (Peshawari chappal)"
     :culture/country "PAK"
     :culture/kind :craft
     :culture/summary "Traditional sandal originating from Pakistan, worn by men casually or formally with the shalwar kameez and often regarded as the national footwear of Pakistan."
     :culture/url "https://en.wikipedia.org/wiki/Kheri_(footwear)"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "pak.craft.truck-art"
     :culture/name "Truck art"
     :culture/country "PAK"
     :culture/kind :craft
     :culture/summary "Elaborate decoration of trucks with paint, calligraphy and ornamentation, especially common in Pakistan and India."
     :culture/url "https://en.wikipedia.org/wiki/Truck_art_in_South_Asia"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "pak.festival.basant"
     :culture/name "Basant"
     :culture/country "PAK"
     :culture/kind :festival
     :culture/summary "Seasonal spring festival celebrated in the Punjab province of Pakistan."
     :culture/url "https://en.wikipedia.org/wiki/Basant_(festival)"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "pak.heritage.mohenjo-daro"
     :culture/name "Mohenjo-daro"
     :culture/country "PAK"
     :culture/kind :heritage
     :culture/summary "Archaeological site in Pakistan, designated a UNESCO World Heritage Site in 1980 -- the first site in South Asia to be so designated."
     :culture/url "https://en.wikipedia.org/wiki/Mohenjo-daro"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}]})

(defn spec-basis [iso3] (get catalog iso3))

(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s)
         missing (remove catalog iso3s)]
     {:requested (count iso3s)
      :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note (str "cloud-itonami-iso3166-pak culture catalog "
                 "(ADR-2607171400 addendum 2, Wave 1): " (count (get catalog "PAK"))
                 " PAK entries, each with a fetched-and-read citation. "
                 "Extend `culture.facts/catalog`, never fabricate an id/url.")})))

(defn by-kind [iso3 kind]
  (filterv #(= (:culture/kind %) kind) (spec-basis iso3)))
