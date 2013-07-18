#version 140

in vec3 vertexPosition;
in vec3 vertexColour;
uniform mat4 MVP;

out vec3 fragmentColour;

void main() {
    gl_Position = MVP * vec4(vertexPosition, 1);
    fragmentColour = vertexColour;
}
