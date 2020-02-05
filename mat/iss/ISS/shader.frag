#version 430

uniform float     u_time;
uniform vec2      u_resolution;
uniform sampler2D tex_00;

in vec4 gl_FragCoord;
in float A;
in vec3  N;
in vec3  Ng;
in vec3  Cd;
in vec3  uv;

out vec4 fragColor;

void main()
{
	vec3 normal =
		vec3( N.x*0.5+0.5
			, N.y*0.5+0.5
			,-N.z*0.5+0.5 );
	
	fragColor = vec4( Cd.x, Cd.y, Cd.z, A );
	//fragColor = vec4( normal, 1.0);
}
