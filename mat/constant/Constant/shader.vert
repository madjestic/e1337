#version 430 core

layout(location = 0) in float alpha;
layout(location = 1) in vec3 color;
layout(location = 2) in vec3 normal;
layout(location = 3) in vec3 uvCoords;
layout(location = 4) in vec3 vPosition;

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

	A  = alpha;
	N  = normal;
	Cd = color;
	uv = uvCoords;
	vec4 position = vec4(vPosition,1);

	position    = transform * position;
	gl_Position = persp * viewM44 * (position + (camera)[3]);
}
