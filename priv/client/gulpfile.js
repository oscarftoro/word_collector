var gulp         = require('gulp'),
    gutil        = require('gulp-util'),
    coffee       = require('gulp-coffee'),
    ngClassify   = require('gulp-ng-classify'),
    ngmin        = require('gulp-ngmin'),
    ngAnnotate   = require('gulp-ng-annotate'),
    karma        = require('karma').server,
    livereload   = require('gulp-livereload'),
    lr           = require('tiny-lr'),
    server       = lr();

var coffeeSources = ['components/coffee/*.coffee'],
    js_index = ['js/*.js','*.html'];

gulp.task('watch',function(){
  var server = livereload();  
  gulp.watch(coffeeSources, ['scripts']);
  gulp.watch(js_index, function(e){
      server.changed(e.path);
  });
});

gulp.task('test',function(done){
    karma.start({
           configFile: __dirname + '/karma.conf.coffee',
           singleRun: true
        },done);
});

gulp.task('tdd',function(done){
    karma.start({
        configFile: __dirname + '/karma.conf.coffee'
    },done);
});

gulp.task('scripts',function(){
    var options = {

    };
    return gulp
        .src(coffeeSources)
        .pipe(ngClassify(options))
        .pipe(gulp.dest('ng-classified'))
        .pipe(coffee({bare: true})
        .on('error',gutil.log))
        .pipe(ngmin())
        .pipe(gulp.dest('js'));

});
gulp.task('default',['scripts','watch','tdd']);
gulp.task('w',['scripts','watch']);
