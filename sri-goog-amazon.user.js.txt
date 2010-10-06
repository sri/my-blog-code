// ==UserScript==
// @name           Sri's Stuff
// @namespace      http://defcraft.org
// @description    Sri's Stuff
// @include        *
// ==/UserScript==

// Created-On: Jun 28, 2007
// Description:
//
// o Selection Search:
//   Make a selection and hit "G" or "A" to
//   search either Google or Amazon. Search opens
//   in background Tab.


(function () {

  document.addEventListener('keypress',  kpHandler, false);

  function kpHandler(event) {
    var letter = String.fromCharCode(event.charCode);
    var sel = document.getSelection();

    if (!event.altKey || sel.length == 0)
      return;

    switch (letter) {
    case "a":
      doAmazonSearch(sel); 
      break;

    case "g":
      doGoogleSearch(sel);
      break;
    }
  }


  // == User actions ============================

  function doAmazonSearch(sel) {
    var url = 'http://www.amazon.com/exec/obidos/external-search/' + 
              '?field-keywords=' + encodeURIComponent(sel) +
              '&mode=blended&tag=mozilla-20&sourceid=Mozilla-search';

    GM_openInTab(url);
  }


  function doGoogleSearch(sel) {
    var url = 'http://www.google.com/search?q=' +
              encodeURIComponent(sel) +
              '&ie=utf-8&oe=utf-8&aq=t' +
              '&rls=org.mozilla:en-US:official&client=firefox-a';

    GM_openInTab(url);
  }


})();
