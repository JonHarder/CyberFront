const gulp = require('gulp');
const elm = require('gulp-elm');

const DEVELOPMENT = "development";
const PRODUCTION = "production";

const build_mode = process.env.NODE_ENV ? process.env.NODE_ENV : DEVELOPMENT;

let options = {
    output: null,
    optimize: null,
};

switch(build_mode) {
    case DEVELOPMENT:
        options.output = "src/";
        options.optimize = false;
        break;
    case PRODUCTION:
        options.output = "dist/";
        options.optimize = true;
        break;
    default:
        console.error(`unrecognized build mode: ${build_mode}`);
        process.exit(1);
}


gulp.task('elm-bundle', () => {
    return gulp.src('src/**/Main.elm', { optimize: options.optimize })
        .pipe(elm.bundle('elm.js'))
        .pipe(gulp.dest(options.output));
});

gulp.task('html', () => {
    return gulp.src('src/index.html')
        .pipe(gulp.dest(options.output));
});

gulp.task('assets', () => {
    return gulp.src('src/assets/**/*')
        .pipe(gulp.dest(options.output + '/assets'));
});

gulp.task('js', () => {
    return gulp.src('src/index.js')
        .pipe(gulp.dest(options.output));
});


console.log(`building in ${build_mode} mode`);
gulp.task('default', ['elm-bundle', 'html', 'js', 'assets']);
