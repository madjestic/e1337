#version 450

#ifdef GL_ES
precision mediump float;
#endif

#define PI     3.14159265358979323846
#define TWO_PI 6.28318530718

layout(location = 0) out vec4 diffuseColor;

uniform float u_time;
uniform vec2 u_mouse;
uniform vec2 u_resolution;

in  vec4 gl_FragCoord;
in  vec3 uv;
out vec4 fragColor;

float circle(in vec2 _st, in float _radius)
{
	vec2 l = _st - vec2(0.5);
	return 1. - smoothstep(_radius - _radius*0.01,
						   _radius + _radius*0.01,
		                   dot(l,l)*4.0);
}

float box(vec2 _st, vec2 _size, float _smoothEdges)
{
	_size = vec2(0.5) - _size * 0.5;
	vec2 aa = vec2(_smoothEdges * 0.5);
	vec2 uv = smoothstep(_size, _size+aa, _st);
	uv     *= smoothstep(_size, _size+aa, vec2(1.0) - _st);
	return uv.x * uv.y;
}

float random(vec2 st) {
	return fract(sin(dot( st.xy
						, vec2 (12.9897, 78.237)))
				 * 43758.5453123+u_time*0.0005);
}

void main() {
	vec2 st    = gl_FragCoord.xy/u_resolution.xy;
	vec3 color = vec3(0.0);

	float rnd  = random (st);
	float rnd2 = random (st*3);
	color = vec3(circle(st+vec2(rnd, rnd2)*.05+vec2(-0.025), 0.5));
	//color = vec3(box(st, vec2(0.95, 0.85), 0.05));
	//color = vec3(st,0.0);
	color *= vec3(rnd);
	//color = vec3(st+vec2(rnd, rnd)*0.1, .0);
	//color = vec3(step(st.x,st.y));
	//color *= vec3(1,.3,.5); // pink


	fragColor = vec4(color, 1.0);
}
