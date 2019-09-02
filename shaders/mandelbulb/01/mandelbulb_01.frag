#version 450
#define V vec2(cos(r),sin(r))

in  vec3 fragCoord;
// in  float time;
out vec4 fragColor;

uniform float u_time;


float t,r,c,d,e = .005,p;
vec3 z, w, o, f;

// #define M d=2.;z=o;for(int i=0;i<10;i++)p=pow(r=length(z),8.),r<4.?d=p/r*8.*d+2.,r=8.*acos(z.z/r)+t*25.,f.xy=V,r=8.*atan(z.y,z.x),z=p*vec3(f.y*V,f)+o:z;r*=log(r)/d

void M() {
    d = 2.;
    z = o;
    for (int i=0; i<10; i++)
        p = pow( r = length(z), 8.),
        r < 4. ?
        	d = p/r*8.*d  +2.,
        	r = 8.* acos(z.z/r) + t*25.,
        	f.xy = V,
        	r = 8.* atan(z.y,z.x),
        	z = p*vec3(f.y*V,f ) + o
        : z;
    r *= log(r)/d;

}

void main() 
{
	vec3  iResolution = vec3(1024, 1024, 1.0);
	float iGlobalTime = u_time;
	vec2 U = vec2(fragCoord.x, fragCoord.y);
	
    z = iResolution, o = z-z, r = t = 10.0;.1*u_time;//iDate.w; // initialization on globals now forbidden
    w = vec3((U-z.xy*.5)/z.y, 1);
    w.xz *= mat2(o.zx=V,-o.x,o.z);
    o /= -.4;
    for (int i = 0; i < 90; i++) {
        M();
        c = r;
        c > e ? o += c*w : o;
    }
    o.x += e;
    M();
    //O = O-O + 2.* abs( c - r ) / e ;
	float val = 2.* abs( c - r ) / e ;
	fragColor = vec4(val,val,val, 1.0);
}
