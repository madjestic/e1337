#version 450

uniform float u_time;
uniform vec2 u_mouse;
uniform vec2 u_resolution;

in  vec4 gl_FragCoord;
in  vec3 uv;
out vec4 fragColor;

void main()
{
	vec2 st = gl_FragCoord.xy/u_resolution.xy;
	gl_FragColor = vec4(st, 0.0, 0.0);
}
