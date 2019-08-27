#version 450

uniform float u_time;
uniform vec2 u_mouse;
uniform vec2 u_resolution;

in  vec4 gl_FragCoord;
in  vec3 uv;
out vec4 fragColor;

void main()
{
	vec2 st    = gl_FragCoord.xy/u_resolution.xy;
	st.x      *= u_resolution.x/u_resolution.y;
	vec3 color = vec3(0.0);
	float d    = 0.0;

	// Remap the space to -1. to 1.
	st = st *2. -1.;

	// Make the distance field
	float offset = (sin(u_time/1000)*0.5+0.5);
	//d = length( abs(st) - offset );
	d = length( min(abs(st)- offset, 0.));
	//d = length( max(abs(st)- offset, 0.));


	// Visualize the distance field
	
	//gl_FragColor = vec4(vec3(fract(d*10.0)), 1.0);
	//gl_FragColor = vec4(vec3(smoothstep(0.15,0.2,d)), 1.0);
	//gl_FragColor = vec4(vec3( step(.3,d) * step(d,.5)),1.0);
	gl_FragColor = vec4(vec3( smoothstep(.3,.4,d)* smoothstep(.6,.5,d)) ,1.0);
}
