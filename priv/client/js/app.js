var Home;
Home = function () {
  function Home($scope, $log) {
    $scope.name = 'Oscarin';
  }
  return Home;
}();
angular.module('app').controller('homeController', [
  '$scope',
  '$log',
  Home
]);