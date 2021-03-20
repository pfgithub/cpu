require('esbuild').build({
    entryPoints: ['src/cpu_logic.ts'],
    bundle: true,
    outfile: 'dist/out.js',
}).catch(() => process.exit(1))