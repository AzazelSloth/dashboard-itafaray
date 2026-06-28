# =====================================================================
#  i18n custom (FR / EN / MG) — inspiré de MDG_framagit.
#  Le package shiny.i18n casse sur Shiny >= 1.11 ; on implémente donc un
#  i18n maison : dictionnaire JSON injecté en JS, et changement de langue
#  par remplacement des noeuds-texte du DOM (aucun input binding touché).
#
#  Usage UI : i18n$t("clé FR")  -> renvoie la clé FR (rendu par défaut en FR).
#  Usage serveur : i18n_lookup("clé FR", "en") -> traduction côté serveur.
# =====================================================================
suppressPackageStartupMessages({ library(jsonlite) })

I18N_FILE    <- "translations/translation.json"
I18N_LANGS   <- c("fr", "en", "mg")
I18N_DEFAULT <- "mg"
I18N_LABELS  <- c(fr = "FR", en = "EN", mg = "MG")

i18n_dict <- jsonlite::fromJSON(I18N_FILE, simplifyVector = FALSE)
if (is.null(i18n_dict$translation) || length(i18n_dict$translation) == 0)
  warning("translations/translation.json est vide ou mal formé.")

.build_i18n_dicts <- function(entries) {
  fwd <- list(fr = list(), en = list(), mg = list())
  bwd <- list(en = list(), mg = list())
  for (entry in entries) {
    fr_key <- entry$fr
    if (is.null(fr_key) || !nzchar(fr_key)) next
    for (lang in I18N_LANGS) {
      val <- entry[[lang]]; if (is.null(val) || !nzchar(val)) val <- fr_key
      fwd[[lang]][[fr_key]] <- val
    }
    if (!is.null(entry$en) && nzchar(entry$en)) bwd$en[[entry$en]] <- fr_key
    if (!is.null(entry$mg) && nzchar(entry$mg)) bwd$mg[[entry$mg]] <- fr_key
  }
  list(forward = fwd, reverse = bwd)
}
i18n_dicts <- .build_i18n_dicts(i18n_dict$translation)

# i18n$t() : renvoie la clé FR (le swap visuel se fait côté client)
i18n <- list(t = function(key) i18n_lookup(key, I18N_DEFAULT))

# Traduction serveur-side
i18n_lookup <- function(key, lang = "fr") {
  if (is.null(key) || !nzchar(key)) return(key)
  if (!lang %in% I18N_LANGS) lang <- I18N_DEFAULT
  out <- i18n_dicts$forward[[lang]][[key]]
  if (is.null(out)) key else out
}

# Traduction vectorisée (pour les labels d'échelles ggplot : secteurs, niveaux…)
i18n_vec <- function(x, lang = "fr") vapply(as.character(x),
  function(v) i18n_lookup(v, lang), character(1), USE.NAMES = FALSE)

detect_browser_lang <- function(session) {
  if (is.null(session) || is.null(session$request)) return(I18N_DEFAULT)
  accept <- session$request$HTTP_ACCEPT_LANGUAGE
  if (is.null(accept) || !nzchar(accept)) return(I18N_DEFAULT)
  code <- tolower(substr(strsplit(accept, ",", fixed = TRUE)[[1]][1], 1, 2))
  if (code %in% I18N_LANGS) code else I18N_DEFAULT
}

# Sélecteur de langue (pilules FR / EN / MG) — pour l'en-tête
lang_switcher_ui <- function() {
  shiny::tags$li(class = "dropdown lang-switcher-wrap",
    shiny::tags$div(class = "lang-switcher",
      shiny::tags$button(id = "lang_btn_fr", type = "button", class = "lang-pill",
        onclick = "Shiny.setInputValue('lang','fr',{priority:'event'})", "FR"),
      shiny::tags$button(id = "lang_btn_en", type = "button", class = "lang-pill",
        onclick = "Shiny.setInputValue('lang','en',{priority:'event'})", "EN"),
      shiny::tags$button(id = "lang_btn_mg", type = "button", class = "lang-pill active",
        onclick = "Shiny.setInputValue('lang','mg',{priority:'event'})", "MG")))
}

lang_switcher_css <- function() {
  shiny::tags$head(shiny::tags$style(shiny::HTML("
    .lang-switcher-wrap { padding: 10px 16px 0 0; }
    .lang-switcher { display:inline-flex; background:rgba(255,255,255,0.12);
      border-radius:16px; padding:2px; gap:1px; }
    .lang-pill { background:transparent; border:none; color:rgba(255,255,255,0.78);
      padding:4px 11px; font-size:11px; font-weight:600; letter-spacing:0.5px;
      border-radius:14px; cursor:pointer; transition:all 0.15s ease; }
    .lang-pill.active { background:rgba(255,255,255,0.95); color:#1e3a5f; }
    .lang-pill:hover:not(.active) { color:#fff; background:rgba(255,255,255,0.10); }
    .lang-pill:focus { outline:none; }
  ")))
}

# Injection du dictionnaire + handler de swap DOM (compatible Shiny >= 1.11)
lang_switcher_js <- function() {
  payload_json <- jsonlite::toJSON(
    list(forward = i18n_dicts$forward, reverse = i18n_dicts$reverse), auto_unbox = TRUE)
  js <- sprintf("
    (function() {
      var i18nData = %s;
      window._i18nFwd = i18nData.forward; window._i18nBwd = i18nData.reverse;
      window._currentLang = 'mg';
      function setActivePill(code) {
        ['fr','en','mg'].forEach(function(c) {
          var el = document.getElementById('lang_btn_' + c); if (!el) return;
          if (c === code) el.classList.add('active'); else el.classList.remove('active');
        });
      }
      function syncDocumentTitle(lang) {
        var labels = window._i18nFwd[lang] || {};
        document.title = labels['Tableau de bord iTafaray'] || 'Tableau de bord iTafaray';
      }
      function swapLanguage(newLang) {
        if (['fr','en','mg'].indexOf(newLang) === -1) return;
        syncDocumentTitle(newLang);
        if (!newLang || newLang === window._currentLang) { setActivePill(newLang); return; }
        var fromLang = window._currentLang;
        var fwd = window._i18nFwd[newLang] || {};
        var bwd = (fromLang === 'fr') ? null : (window._i18nBwd[fromLang] || {});
        function lookupNew(t) {
          var frKey = (fromLang === 'fr') ? t : bwd[t];
          if (!frKey) return null;
          var out = fwd[frKey]; return (out === undefined) ? null : out;
        }
        var walker = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT, {
          acceptNode: function(n) {
            if (!n.nodeValue) return NodeFilter.FILTER_REJECT;
            if (n.nodeValue.replace(/^\\s+|\\s+$/g,'') === '') return NodeFilter.FILTER_REJECT;
            var p = n.parentNode; if (!p) return NodeFilter.FILTER_REJECT;
            var tag = (p.tagName||'').toLowerCase();
            if (tag==='script'||tag==='style'||tag==='noscript') return NodeFilter.FILTER_REJECT;
            return NodeFilter.FILTER_ACCEPT;
          }
        }, false);
        var nodes = []; while (walker.nextNode()) nodes.push(walker.currentNode);
        nodes.forEach(function(node) {
          var raw = node.nodeValue;
          var leading=(raw.match(/^\\s*/)||[''])[0], trailing=(raw.match(/\\s*$/)||[''])[0];
          var trimmed=raw.replace(/^\\s+|\\s+$/g,''); var fresh=lookupNew(trimmed);
          if (fresh!==null && fresh!==trimmed) node.nodeValue = leading+fresh+trailing;
        });
        window._currentLang = newLang; setActivePill(newLang);
      }
      function translateSubtree(rootNode) {
        if (window._currentLang === 'fr') return;
        if (!rootNode || rootNode.nodeType !== 1) return;
        var fwd = window._i18nFwd[window._currentLang] || {};
        var walker = document.createTreeWalker(rootNode, NodeFilter.SHOW_TEXT, {
          acceptNode: function(n) {
            if (!n.nodeValue) return NodeFilter.FILTER_REJECT;
            if (n.nodeValue.replace(/^\\s+|\\s+$/g,'') === '') return NodeFilter.FILTER_REJECT;
            var p = n.parentNode; if (!p) return NodeFilter.FILTER_REJECT;
            var tag = (p.tagName||'').toLowerCase();
            if (tag==='script'||tag==='style'||tag==='noscript') return NodeFilter.FILTER_REJECT;
            return NodeFilter.FILTER_ACCEPT;
          }
        }, false);
        var nodes = []; while (walker.nextNode()) nodes.push(walker.currentNode);
        nodes.forEach(function(node) {
          var raw = node.nodeValue;
          var leading=(raw.match(/^\\s*/)||[''])[0], trailing=(raw.match(/\\s*$/)||[''])[0];
          var trimmed=raw.replace(/^\\s+|\\s+$/g,''); var fresh=fwd[trimmed];
          if (fresh!==undefined && fresh!==trimmed) node.nodeValue = leading+fresh+trailing;
        });
      }
      function startObserver() {
        if (typeof MutationObserver === 'undefined') return;
        if (!document.body) { document.addEventListener('DOMContentLoaded', startObserver); return; }
        try {
          var obs = new MutationObserver(function(ms) {
            if (window._currentLang === 'fr') return;
            ms.forEach(function(m) {
              for (var i=0;i<m.addedNodes.length;i++) translateSubtree(m.addedNodes[i]);
            });
          });
          obs.observe(document.body, { childList:true, subtree:true });
        } catch (e) {}
      }
      function registerHandlers() {
        if (typeof Shiny === 'undefined' || !Shiny.addCustomMessageHandler) {
          setTimeout(registerHandlers, 50); return;
        }
        Shiny.addCustomMessageHandler('i18n_set_lang', swapLanguage);
        Shiny.addCustomMessageHandler('set_lang_pill', setActivePill);
      }
      syncDocumentTitle(window._currentLang);
      registerHandlers(); startObserver();
    })();
  ", payload_json)
  shiny::tags$head(shiny::tags$script(shiny::HTML(js)))
}
