#version 450

uniform float u_time;
uniform vec2 u_resolution;
uniform sampler2D tex_00;

in  vec4 gl_FragCoord;
in  vec3 uv;
out vec4 fragColor;

void main()
{
  fragColor = vec4(texture(tex_00, vec2(uv.x, uv.y)).rgb, 1.0);
}
