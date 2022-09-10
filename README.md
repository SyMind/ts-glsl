# ts-glsl

https://registry.khronos.org/OpenGL/specs/es/2.0/GLSL_ES_Specification_1.00.pdf

```typescript
import { Parse } from "@codemix/ts-glsl";

const vertex = `
attribute vec3 position;
varying vec2 uv;
void main() {
    gl_Position = vec4(position, 1.0);
    uv = position.xy;
}
`
// Vertex.attributes.position = {type: 'vec3', name: 'position'}
type Vertex = Parse<vertex>;
```
