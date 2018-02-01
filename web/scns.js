'use strict';

var app = angular.module('scns', ['ngSanitize', 'ui.select', 'ngRoute', 'ui.bootstrap']);

app.config(function ($routeProvider) {
    $routeProvider
	.when('/', {
	    templateUrl: 'home.html'
	})

	.when('/stg', {
	    templateUrl: 'stg.html',
	    controller: 'checkDataController'
	})

	.when('/results', {
	    templateUrl: 'results.html',
	    controller: 'checkSynthController'
	})

	.when('/analysis', {
	    templateUrl: 'analysis.html',
	    controller: 'checkStableStatesController'
	})

	.when('/config', {
	    templateUrl: 'config.html'
	})
});

app.filter('propsFilter', function () {
    return function (items, props) {
        var out = [];

        if (angular.isArray(items)) {
            items.forEach(function (item) {
                var itemMatches = false;

                var keys = Object.keys(props);
                for (var i = 0; i < keys.length; i++) {
                    var prop = keys[i];
                    var text = props[prop].toLowerCase();
                    if (item[prop].toString().toLowerCase().indexOf(text) !== -1) {
                        itemMatches = true;
                        break;
                    }
                }

                if (itemMatches) {
                    out.push(item);
                }
            });
        } else {
            // Let the output be the input untouched
            out = items;
        }

        return out;
    };
});

var render = function (networ_json) {
    var visual_style = {
        nodes: {
            borderWidth: 0,
            size: 65,
            color: {
                discreteMapper: {
                    attrName: "sort",
                    entries: [
                      { attrValue: "Class1", value: "#8DD3C7" },
                      { attrValue: "Class2", value: "#FFFFB3" },
                      { attrValue: "Class3", value: "#BEBADA" },
                      { attrValue: "Class4", value: "#FB8072" },
                      { attrValue: "Class5", value: "#80B1D3" },
                      { attrValue: "Class6", value: "#FDB462" },
                      { attrValue: "Class7", value: "#B3DE69" },
                      { attrValue: "Class8", value: "#FCCDE5" },
                      { attrValue: "Class9", value: "#D9D9D9" },
                      { attrValue: "Class10", value: "#BC80BD" },
                      { attrValue: "Class11", value: "#CCEBC5" },
                      { attrValue: "Class12", value: "#FFED6F" }]
                }
            }
        }
    };

    // id of Cytoscape Web container div
    var div_id = "cytoscapeweb";

    // initialization options
    var options = {
        // where you have the Cytoscape Web SWF
        swfPath: "CytoscapeWeb",
        // where you have the Flash installer SWF
        flashInstallerPath: "playerProductInstall"
    };

    // init and draw
    var vis = new org.cytoscapeweb.Visualization(div_id, options);
    vis.draw({ network: networ_json, visualStyle: visual_style, panZoomControlVisible: false });
};

app.controller('DemoCtrl', function ($scope, $http, $location, $rootScope) {
    $scope.availableColors = [];
    $scope.multipleDemo = {};
    $scope.multipleDemo.colors1 = [];
    $scope.multipleDemo.colors2 = [];

    $scope.setcolor = function (c) {
        return { 'color': c.color }
    }

    $http.get('data')
      .then(function (response) {
          $scope.availableColors = response.data.cellClasses;
          $scope.parameters = response.data.parameters;
          $scope.multipleDemo.colors1 = response.data.initialClasses;
          $scope.multipleDemo.colors2 = response.data.targetClasses;

          render(response.data.stg);

      }, function (response) {
      });

    $scope.synth = function () {
        var fd = new FormData();

        $http.post('synthesise').
               then(function (response) {
                   $location.path('results');
                   $rootScope.synthClicked = true;
                   $rootScope.completeModelFound = false;
               }, function (response) {
               });
    };

    $scope.updateActivators = function (g, v) {
        $http.post('updateActivators', { gene: g, value: v }).
            then(function (response) {
            }, function (response) {
            });
    };
    $scope.updateRepressors = function (g, v) {
        $http.post('updateRepressors', { gene: g, value: v }).
            then(function (response) {
            }, function (response) {
            });
    };
    $scope.updateThreshold = function (g, v) {
        $http.post('updateThreshold', { gene: g, value: v }).
            then(function (response) {
            }, function (response) {
            });
    };

    $scope.addInitialCellClass = function (c) {
        $http.post('addInitialCellClass', c).
            then(function (response) {
            }, function (response) {
            });
    };
    $scope.removeInitialCellClass = function (c) {
        $http.post('removeInitialCellClass', c).
            then(function (response) {
            }, function (response) {
            });
    };
    $scope.addTargetCellClass = function (c) {
        $http.post('addTargetCellClass', c).
            then(function (response) {
            }, function (response) {
            });
    };
    $scope.removeTargetCellClass = function (c) {
        $http.post('removeTargetCellClass', c).
            then(function (response) {
            }, function (response) {
            });
    };

});

app.controller('checkDataController', function ($rootScope, $location) {
    if (!$rootScope.data) {
        $location.path('/');
    }
});

app.controller('checkSynthController', function ($rootScope, $location) {
    if (!$rootScope.data) {
        $location.path('/');
    }
    else if (!$rootScope.synthClicked) {
        $location.path('stg');
    }
});


app.controller('checkStableStatesController', function ($rootScope, $location) {
    if (!$rootScope.data) {
        $location.path('/');
    }
    else if (!$rootScope.synthClicked) {
        $location.path('stg');
    }
    else if (!$rootScope.completeModelFound) {
        $location.path('results');
    }
});

app.directive('customOnChange', function () {
    return {
        restrict: 'A',
        link: function (scope, element, attrs) {
            var onChangeFunc = scope.$eval(attrs.customOnChange);
            element.bind('change', onChangeFunc);
        }
    };
});

app.controller('Upload', function ($scope, $rootScope, $http, $timeout, $location) {
    $scope.uploadFile = function (event) {
        var fd = new FormData();
        fd.append('file', event.target.files[0]);

        $http.post('uploadCSV', fd, { transformRequest: angular.identity, headers: { 'Content-Type': undefined } }).
               then(function (response) {
                   $location.path('stg');
                   $rootScope.data = true;
                   $rootScope.synthClicked = false;
                   $rootScope.completeModelFound = false;
               }, function (response) {
                   alert('fail');
               });
    };
});

function allTrue(obj) {
    for (var o in obj) if (!obj[o]) return false;

    return true;
}

app.controller('TabsDemoCtrl', function ($scope, $rootScope, $timeout, $http) {
    var selectedTab = 0;
    var terminated = {};

    $scope.setSelectedTab = function (i) {
        selectedTab = i;
    };

    var poll = function () {
        $http.get('results')
          .then(function (response) {
              $scope.tabs = response.data.results;
              terminated = response.data.terminated;
              $scope.tabs[selectedTab].active = true;

              if (allTrue(terminated)) {
                  $rootScope.completeModelFound = response.data.completeModelFound;
              }
              else {
                  $timeout(poll, 3000);
              }
          }, function (response) {
          });
    };
    poll();

    $scope.isFinished = function (gene) {
        return terminated[gene];
    };
});

var tempI = 0;

app.controller('Analysis', function ($scope, $http, $timeout, $sce) {
    $scope.getHeatmap = '<h2><i class="fa fa-spinner fa-pulse"></i></h2>';

    $scope.availableGenes = [];
    $scope.perturbations = {};
    $scope.perturbations.kos = [];
    $scope.perturbations.oes = [];

    $http.get('genes')
           .then(function (response) {

               if (response.data.kos.length > 0) {
                   $scope.perturbations.kos = response.data.kos;
               }
               if (response.data.oes.length > 0) {
                   $scope.perturbations.oes = response.data.oes;
               }
               if (response.data.genes.length > 0) {
                   $scope.availableGenes = response.data.genes;
               }

               for (var i = 0; i < $scope.perturbations.kos.length; i++) {
                   var j = $scope.availableGenes.indexOf($scope.perturbations.kos[i]);
                   if (j > -1) {
                       $scope.availableGenes.splice(j, 1);
                   }
               }

               for (var i = 0; i < $scope.perturbations.oes.length; i++) {
                   var j = $scope.availableGenes.indexOf($scope.perturbations.oes[i]);
                   if (j > -1) {
                       $scope.availableGenes.splice(j, 1);
                   }
               }
           }, function (response) {
           });

    var poll = function () {
        $http.get('stableStatesTerminated')
          .then(function (response) {
              if (response.data === "true") {
                  tempI++;
                  $scope.getHeatmap = $sce.trustAsHtml('<img src="stableStatesHeatmap.png?' + tempI + '" style="width: 100%">');
              }
              else {
                  $timeout(poll, 1000);
              }
          }, function (response) {
          });
    };
    
    $scope.addKO = function (g) {
        $scope.getHeatmap = '<h2><i class="fa fa-spinner fa-pulse"></i></h2>';
        var i = $scope.availableGenes.indexOf(g);
        if (i > -1) {
            $scope.availableGenes.splice(i, 1);
        }

        $http.post('stableStates', $scope.perturbations).
            then(function (response) {
                poll();
            }, function (response) {
            });
    };
    $scope.removeKO = function (g) {
        $scope.getHeatmap = '<h2><i class="fa fa-spinner fa-pulse"></i></h2>';
        $scope.availableGenes.push(g);
        $scope.availableGenes.sort();
        $http.post('stableStates', $scope.perturbations).
             then(function (response) {
                 poll();
             }, function (response) {
             });
    };
    $scope.addOE = function (g) {
        $scope.getHeatmap = '<h2><i class="fa fa-spinner fa-pulse"></i></h2>';
        var i = $scope.availableGenes.indexOf(g);
        if (i > -1) {
            $scope.availableGenes.splice(i, 1);
        }

        $http.post('stableStates', $scope.perturbations).
            then(function (response) {
                poll();
            }, function (response) {
            });
    };
    $scope.removeOE = function (g) {
        $scope.getHeatmap = '<h2><i class="fa fa-spinner fa-pulse"></i></h2>';
        $scope.availableGenes.push(g);
        $scope.availableGenes.sort();
        $http.post('stableStates', $scope.perturbations).
             then(function (response) {
                 poll();
             }, function (response) {
             });
    };

    poll();
});

app.controller('CloudConfig', function ($scope, $http) {
    $scope.azure = {};
    $scope.azure.cloudProvisioned = "false";
    $scope.azure.runOnCloud = "false";

    $http.get('cloudProvisioned')
         .then(function (response) {
             $scope.azure.cloudProvisioned = response.data;
         }, function (response) {
         });

    $http.get('runOnCloud')
         .then(function (response) {
             $scope.azure.runOnCloud = response.data;
         }, function (response) {
         });

    $scope.updateRunOnCloud = function (c) {
        $http.post('updateRunOnCloud', $scope.azure.runOnCloud)
             .then(function (response) {
             }, function (response) {
                 $scope.azure.runOnCloud = "false";
                 alert('Could not connect to Azure with given settings. If this is your first attempt to connect, please allow a few minutes to provision cluster.');
             });
    };
});