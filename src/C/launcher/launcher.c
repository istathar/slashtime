#include <unistd.h>
#include <sys/types.h>
#include <errno.h>
#include <wait.h>

int main(int argc, char** argv) {
	pid_t pid;
	int status;

	chdir("/home/andrew/src/andrew/slashtime/expedient");
		
	pid = fork();
	if (pid == 0) {
		execlp("/opt/sun-jdk-1.5.0.13/bin/java",
			"/opt/sun-jdk-1.5.0.13/bin/java", 	
			"-client", 
			"-classpath","/home/andrew/src/andrew/java-gnome/primary/tmp/gtk-4.0.jar:tmp/classes",
        		"-Djava.library.path=/home/andrew/src/andrew/java-gnome/primary/tmp:tmp",
        		"com.operationaldynamics.slashtime.Master",
        		NULL);
        	return errno;
	}
	
	wait(&status);
	
	if (WIFEXITED(status)) {
		return 0;
	} else {
		return 1;
	}
}
