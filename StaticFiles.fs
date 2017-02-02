module StaticFiles

open Suave.Filters
open Suave.Operators
open Suave.Files

let staticFiles =
    [ path "/" >=> file "index.html"
      path "/favicon.ico" >=> file "favicon.ico"
      path "/bootstrap.min.css" >=> file "bootstrap.min.css"
      path "/cover.css" >=> file "cover.css"
      path "/select.css" >=> file "select.css"
      path "/select2.css" >=> file "select2.css"
      path "/font-awesome.min.css" >=> file "font-awesome.min.css"
      path "/AC_OETags.min.js" >=> file "AC_OETags.min.js"
      path "/cytoscapeweb.min.js" >=> file "cytoscapeweb.min.js"
      path "/json2.min.js" >=> file "json2.min.js"
      path "/angular.min.js" >=> file "angular.min.js"
      path "/angular-sanitize.min.js" >=> file "angular-sanitize.min.js"
      path "/angular-route.min.js" >=> file "angular-route.min.js"
      path "/select.js" >=> file "select.js"
      path "/ui-bootstrap.min.js" >=> file "ui-bootstrap.min.js"
      path "/scns.js" >=> file "scns.js"
      path "/analysis.html" >=> file "analysis.html"
      path "/config.html" >=> file "config.html"
      path "/home.html" >=> file "home.html"
      path "/results.html" >=> file "results.html"
      path "/stg.html" >=> file "stg.html"
      path "/CytoscapeWeb.swf" >=> file "CytoscapeWeb.swf"
      path "/playerProductInstall.swf" >=> file "playerProductInstall.swf" ]
