import resolve from 'rollup-plugin-node-resolve'
import babel from 'rollup-plugin-babel'
import commonjs from 'rollup-plugin-commonjs'
import replace from 'rollup-plugin-replace'
import { terser } from 'rollup-plugin-terser'
// import postcss from 'rollup-plugin-postcss'

// `npm run build` -> `production` is true
// `npm run dev` -> `production` is false

const production = !process.env.ROLLUP_WATCH

const plugins = [
    resolve(), // tells Rollup how to find date-fns in node_modules
    commonjs({
        include: 'node_modules/**',
        namedExports: {
            // 'src/bs4/index.js': [ 'Bs4Containers' ],
            'node_modules/react/index.js': [
                'Component',
                'PureComponent',
                'Fragment',
                'Children',
                'createElement',
                'createFactory'
            ],
            'node_modules/react-is/index.js': [
                'isValidElementType',
            ]
        }
    }), // converts date-fns to ES modules
    babel({
        exclude: 'node_modules/**'
    }),
    replace({
        'process.env.NODE_ENV': JSON.stringify( 'production' )
    }),
    production && terser() // minify, but only in production
]

export default [
    {
        input: 'src/index.js',
        output: {
            file: '../static/js/bundle.js',
            format: 'umd',
            // format: 'iife',
            // immediately-invoked function expression
            // suitable for <script> tags
            sourcemap: false,
            name: "_ui"
        },
        plugins: plugins
    },
]