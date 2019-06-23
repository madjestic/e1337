#version 430 core

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec3 uvCoords;
uniform float u_time;
uniform mat4  persp;
uniform mat4  camera;
uniform mat4  transform;

// Output data ; will be interpolated for each fragment.
out vec3 fragCoord;
out float time;

void main()
{
	mat4 viewM44 =
		mat4( camera[0]
			, camera[1]
			, camera[2]
			, vec4(0,0,0,1));
	
	gl_Position = //camera * persp * transform * vPosition;
		persp * viewM44 * (vPosition + camera[3]);
	fragCoord   = uvCoords;
	time        = u_time;
}
