# ts-glsl

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
