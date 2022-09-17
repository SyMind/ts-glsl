# ts-glsl

In accordance with [GLSL ES Specification 1.0](https://registry.khronos.org/OpenGL/specs/es/2.0/GLSL_ES_Specification_1.00.pdf).

```typescript
import { Parse } from 'ts-glsl';

const vertex = `
attribute vec3 position;
varying vec2 uv;
void main() {
    gl_Position = vec4(position, 1.0);
    uv = position.xy;
}
`

type Vertex = Parse<vertex>;
```

# License

[MIT](https://github.com/SyMind/ts-glsl/blob/main/LICENSE)
