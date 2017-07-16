@echo off
rem in project directory: i.e., stack_root\Strat
rem npm install typescript
rem npm install -g browserify
rem npm install tsify
rem npm install jquery

rem browserify src\Strat-web\Static\gameboard.ts -p [ tsify --noImplicitAny ] > src\Strat-Web\Static\bundle.js
call browserify src\Strat-web\Static\gameboard.ts -p [ tsify --strict ] > src\Strat-Web\Static\bundle.js

rem in html file: <script src="bundle.js"></script>
