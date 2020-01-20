#version 450

uniform float u_time;
uniform vec2 u_resolution;
uniform sampler2D tex_00;

in vec4 gl_FragCoord;
in float A;
in vec3  N;
in vec3  Cd;
in vec3  uv;

out vec4 fragColor;

void main()
{
	///////////////////////////////////////////////
	//// this seems to be more or less equivalents:
	///////////////////////////////////////////////
	
	// fragColor = vec4( dot(vec3(1,0,0),N)*0.5+0.5
	// 				, dot(vec3(0,1,0),N)*0.5+0.5
	// 				, (-N.z*0.5+0.5)//dot(vec3(0,0,-1),(N))
	// 				, 0.0);
	fragColor = vec4( (N.x*0.5+0.5) 
					, (N.y*0.5+0.5) 
					,(-N.z*0.5+0.5)
					, 1.0);
}
