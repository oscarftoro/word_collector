class Home
  constructor: ($scope, $log) ->
    $scope.name = 'Oscarin'


angular.module('app')
.controller('homeController', ['$scope', '$log', Home])