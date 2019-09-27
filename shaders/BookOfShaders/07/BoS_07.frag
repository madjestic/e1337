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

float plot(vec2 st, float pct, float border)
{
  return  smoothstep( pct-border, pct, st.y) -
          smoothstep( pct, pct+border, st.y);
}

float circle(in vec2 _st, in float _radius)
{
	vec2 l = _st - vec2(0.5);
	return 1. - smoothstep(_radius - _radius*0.01,
						   _radius + _radius*0.01,
		                   dot(l,l)*4.0);
}

vec2 rotate2D(vec2 _st, float _angle)
{
	_st -= 0.5;
	_st = mat2(cos(_angle), -sin(_angle),
		       sin(_angle),  cos(_angle)) * _st;
	_st += 0.5;
	return _st;
}

float box(vec2 _st, vec2 _size, float _smoothEdges)
{
	_size = vec2(0.5) - _size * 0.5;
	vec2 aa = vec2(_smoothEdges * 0.5);
	vec2 uv = smoothstep(_size, _size+aa, _st);
	uv     *= smoothstep(_size, _size+aa, vec2(1.0) - _st);
	return uv.x * uv.y;
}

vec2 movingTiles_(vec2 _st, float _zoom, float _speed){
    _st *= _zoom;
    float time = u_time*_speed;
    if( fract(time)>0.5 ){
        if (fract( _st.y * 0.5) > 0.5){
            _st.x += fract(time)*2.0;
        } else {
            _st.x -= fract(time)*2.0;
        }
    } else {
        if (fract( _st.x * 0.5) > 0.5){
            _st.y += fract(time)*2.0;
        } else {
            _st.y -= fract(time)*2.0;
        }
    }
	//return _st;
    return fract(_st);
}


vec2 movingTiles(vec2 _st, float _zoom, float _speed){
    _st *= _zoom;
    float time = u_time*_speed;
    if( fract(time)>0.5 ){
        if (fract( _st.y * 0.5) > 0.5){
            _st.x += fract(time)*2.;
        } else {
            _st.x -= fract(time)*2.;
        }
    } else {
        if (fract( _st.x * 0.5) > 0.5){
            _st.y += fract(time)*2.;
        } else {
            _st.y -= fract(time)*2.;
        }
    }
    return fract(_st);
}

vec2 rotateTilePattern(vec2 _st){

    //  Scale the coordinate system by 2x2
    _st *= 2.0;

    //  Give each cell an index number
    //  according to its position
    float index = 0.0;
    index += step(1., mod(_st.x,2.0));
    index += step(1., mod(_st.y,2.0))*2.0;

    //      |
    //  2   |   3
    //      |
    //--------------
    //      |
    //  0   |   1
    //      |

    // Make each cell between 0.0 - 1.0
    _st = fract(_st);

    // Rotate each cell according to the index
    if(index == 1.0){
        //  Rotate cell 1 by 90 degrees
        _st = rotate2D(_st,PI*0.5);
    } else if(index == 2.0){
        //  Rotate cell 2 by -90 degrees
        _st = rotate2D(_st,PI*-0.5);
    } else if(index == 3.0){
        //  Rotate cell 3 by 180 degrees
        _st = rotate2D(_st,PI);
    }

    return _st;
}

vec2 tile(vec2 _st, float _zoom) {
	_st *= _zoom;
	return fract(_st);
}

void main()
{
	vec2 st    = gl_FragCoord.xy/u_resolution.xy;
	vec3 color = vec3(0.0);

	//st = movingTiles(st, 1.5, 0.0002);
	//st = rotateTilePattern(st);
	//st = tile(st, 2);
	st = rotate2D(st, PI*u_time*0.0002);    // rotate
	st = movingTiles(st, 2., -0.0002);
	st = rotateTilePattern(-st);
	st = rotate2D(st, PI*u_time*0.0001);    // rotate
	st = movingTiles(st, 2., 0.0001);
		
	//color = vec3(circle(st, 0.5));
	//color = vec3(box(st, vec2(0.95, 0.85), 0.05));
	//color = vec3(st,0.0);
	color = vec3(step(st.x,st.y));
	//color *= vec3(1,.3,.5); // pink

	fragColor = vec4(color, 1.0);
}
