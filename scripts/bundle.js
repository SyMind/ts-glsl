// @ts-check

const fs = require('fs');
const path = require('path');

const sources = ['helper.ts', 'ast.ts', 'string.ts', 'index.ts']
    .map(file => path.resolve(__dirname, '../src', file))
    .map(loadFile)
const demo = `
const vertex = \`
attribute vec3 position;
varying vec2 uv;
void main() {
    gl_Position = vec4(position, 1.0);
    uv = position.xy;
}
\`

type AST = Parse<typeof vertex>;

type Attributes = GetProgramAttributes<AST>
`

const outputs = [
    '// # Usage',
    demo,
    `
    /**
     * ========================================================================================
     * 
     * 
     *                        END OF EXAMPLES, START OF IMPLEMENTATION
     * 
     * 
     * ========================================================================================
     */
    `
        .split(/\n/)
        .map((line) => line.trim())
        .join('\n'),
    ...sources,
];

console.log(outputs.join('\n\n'));

/**
 * @param {string} dir The path to search in.
 */
function* findFiles(dir) {
    for (const name of fs.readdirSync(dir)) {
        const filename = path.join(dir, name);
        if (name.endsWith('.ts')) {
            yield filename;
        } else if (fs.statSync(filename).isDirectory()) {
            yield* findFiles(filename);
        }
    }
}

/**
 * @param {string} filename
 */
function loadFile(filename) {
    const raw = fs.readFileSync(filename, 'utf-8');
    const content = raw
        .replace(/export\s+type/g, 'type')
        .replace(/export\s+\*\s+from\s+'(.*)';?/g, '')
        .replace(/import\s+\{([\s\S]*)\}\s+from\s+'(.*)';?/g, '')
        .trim();
    return content;
}
