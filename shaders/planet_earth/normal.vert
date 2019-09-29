#version 430 core

layout(location = 0) in float alpha;
layout(location = 1) in vec3 color;
layout(location = 2) in vec3 normal;
layout(location = 3) in vec3 uvCoords;
layout(location = 4) in vec4 vPosition;

uniform mat4  persp;
uniform mat4  camera;
uniform mat4  transform;

// Output data ; will be interpolated for each fragment.
out float A;
out vec3  N;
out vec3  Cd;
out vec3  uv;

void main()
{
	mat4 viewM44 =
		mat4( camera[0]
			, camera[1]
			, camera[2]
			, vec4(0,0,0,1));

	mat3 viewM33 =
		mat3( camera[0].xyz
			, camera[1].xyz
			, camera[2].xyz );

	mat3 perspM33 =
		mat3 ( persp[0].xyz
			 , persp[1].xyz
			 , persp[2].xyz );
	
	A  = alpha;
	N  = perspM33 * viewM33 * (normal);
	Cd = color;
	uv = uvCoords;
	gl_Position = persp * viewM44 * (vPosition + camera[3]);
}
