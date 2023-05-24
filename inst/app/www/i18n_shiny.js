$(document).ready(function() {
  // If the local version is used there is no need to parse it
  //var local = "www/i18n_locales.json"
  var local = "https://raw.githubusercontent.com/marton-balazs-kovacs/TransparencyChecklistGolem/master/inst/app/www/i18n_locales.json"
    $.get(local, function(data) {
        i18next.init({
            lng: "en",
            resources: JSON.parse(data)
        }, function(err, t) {
            jqueryI18next.init(i18next, $);
            $("html").localize();
        });
    })
})