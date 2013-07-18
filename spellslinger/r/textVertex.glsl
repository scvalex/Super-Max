#version 140

in vec2 vertexPosition;
in vec2 uvPosition;
uniform mat4 MVP;

out vec2 UV;

void main() {
    gl_Position = MVP * vec4(vertexPosition, 0.2, 1);
    UV = uvPosition;
}
