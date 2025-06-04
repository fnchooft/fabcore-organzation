

# Implementation of an inteface-list

Many examples exist on how to model network equipment, and many need
a data-structure in C to fill these models by using an API.

This folder contains an example on how to structure a list of interfaces.

We use the [stb](https://github.com/nothings/stb)-library.


## Contents

```makefile
# Makefile for STB Data Structures example
TARGET = network_interfaces
STB_URL = https://raw.githubusercontent.com/nothings/stb/master/stb_ds.h
STB_HEADER = stb_ds.h

STB_SURL = https://raw.githubusercontent.com/nothings/stb/master/stb_sprintf.h
STB_SHEADER = stb_sprintf.h

.PHONY: all clean deps

all: deps $(TARGET)

# Download stb_ds.h if missing
deps:
	@if [ ! -f "$(STB_HEADER)" ]; then \
		echo "Downloading $(STB_HEADER)..."; \
		curl -s -o $(STB_HEADER) $(STB_URL) || wget -q -O $(STB_HEADER) $(STB_URL); \
	fi
	@if [ ! -f "$(STB_SHEADER)" ]; then \
		echo "Downloading $(STB_SHEADER)..."; \
		curl -s -o $(STB_SHEADER) $(STB_SURL) || wget -q -O $(STB_SHEADER) $(STB_SURL); \
	fi

# Compile the program
$(TARGET): main.c
	$(CC) -std=c99 -O2 -Wall -Wextra -o $@ $<

# Compile the program
string_test: string_test.c
	$(CC) -std=c99 -O2 -Wall -Wextra -o $@ $<

clean:
	$(RM) $(TARGET)

distclean: clean
	$(RM) $(STB_HEADER)
	

```

```c
#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"
#include <stdio.h>
#include <string.h>

// --- Interface Status Enum ---
typedef enum {
    INTERFACE_UP = 1,       // Ready to pass packets
    INTERFACE_DOWN = 2,     // Not ready + not in test mode
    INTERFACE_TESTING = 3   // In test mode
} InterfaceStatus;

const char* status_to_string(InterfaceStatus s) {
    static const char* strings[] = {
        [INTERFACE_UP]      = "UP (Ready to pass packets)",
        [INTERFACE_DOWN]    = "DOWN (Not ready)",
        [INTERFACE_TESTING] = "TESTING (In test mode)"
    };
    return strings[s];
}

// --- Data Structures ---
typedef struct {
    int id;
    int vlan;
    char type[32];  // "access", "trunk", etc.
} SubInterface;

typedef struct {
    char name[64];
    char desc[128];
    InterfaceStatus status;
    SubInterface* subinterfaces; // stb_ds dynamic array
} NetworkInterface;

// --- Core Functions ---
void add_subinterface(NetworkInterface* intf, int id, int vlan, const char* type) {
    SubInterface sub = {.id = id, .vlan = vlan};
    strncpy(sub.type, type, sizeof(sub.type)-1);
    arrput(intf->subinterfaces, sub);
}

void clear_subinterfaces(NetworkInterface* intf) {
    if (intf && intf->subinterfaces) {
        arrfree(intf->subinterfaces);
        intf->subinterfaces = NULL;
    }
}

void remove_subinterfaces_by_vlan(NetworkInterface* intf, int vlan) {
    if (!intf || !intf->subinterfaces) return;
    
    SubInterface* filtered = NULL;
    for (int i = 0; i < arrlen(intf->subinterfaces); i++) {
        if (intf->subinterfaces[i].vlan != vlan) {
            arrput(filtered, intf->subinterfaces[i]);
        }
    }
    
    arrfree(intf->subinterfaces);
    intf->subinterfaces = filtered;
}

void print_interface(const NetworkInterface* intf) {
    printf("┌─ %s [%s]\n│  Status: %s\n│  Desc: %s\n", 
           intf->name, 
           arrlen(intf->subinterfaces) ? "Composite" : "Simple",
           status_to_string(intf->status),
           intf->desc);
    
    for (int i = 0; i < arrlen(intf->subinterfaces); i++) {
        printf("├── Subif %d: VLAN %d (%s)\n", 
               intf->subinterfaces[i].id,
               intf->subinterfaces[i].vlan,
               intf->subinterfaces[i].type);
    }
    printf("└────────────────\n");
}

// --- Main Program ---
int main() {
    printf("=== Network Interface Demo ===\n\n");
    
    // Create interface with subinterfaces
    NetworkInterface eth0 = {
        .name = "eth0", 
        .desc = "Main Ethernet port",
        .status = INTERFACE_UP
    };
    add_subinterface(&eth0, 1, 100, "access");
    add_subinterface(&eth0, 2, 200, "trunk");
    add_subinterface(&eth0, 3, 100, "hybrid");
    
    // Create wireless interface
    NetworkInterface wlan0 = {
        .name = "wlan0",
        .desc = "Wireless interface (testing mode)",
        .status = INTERFACE_TESTING
    };
    add_subinterface(&wlan0, 1, 300, "access");
    
    // Print initial state
    printf("Initial interfaces:\n");
    print_interface(&eth0);
    print_interface(&wlan0);
    
    // Demo VLAN removal
    printf("\nRemoving VLAN 100 from eth0...\n");
    remove_subinterfaces_by_vlan(&eth0, 100);
    print_interface(&eth0);
    
    // Demo clear
    printf("\nClearing all wlan0 subinterfaces...\n");
    clear_subinterfaces(&wlan0);
    print_interface(&wlan0);
    
    // Cleanup
    clear_subinterfaces(&eth0);
    clear_subinterfaces(&wlan0);
    
    return 0;
}
```

```c
#define STB_SPRINTF_IMPLEMENTATION
#include "stb_sprintf.h"  // Single header version
#include <stdio.h>

int main() {
    char buffer[100];
    
    // 1. Basic formatting (like sprintf)
    stbsp_sprintf(buffer, "Hello, %s! You have %d messages.", "User", 5);
    puts(buffer);  // Output: "Hello, User! You have 5 messages."

    // 2. Safer bounds checking
    stbsp_snprintf(buffer, sizeof(buffer), "Pi ≈ %.5f", 3.1415926535);
    puts(buffer);  // Output: "Pi ≈ 3.14159"

    // 3. Advanced features (no standard library equivalent)
    stbsp_sprintf(buffer, "Hex: %08x | Binary: %b", 255, 255);
    puts(buffer);  // Output: "Hex: 000000ff | Binary: 11111111"

    // 4. Custom formatting (no allocations)
    stbsp_sprintf(buffer, "Commas: %'d", 1000000);
    puts(buffer);  // Output: "Commas: 1,000,000"
    
    return 0;
}

```

## Download links

{% file src="Makefile" %}Makefile{% endfile %}
{% file src="main.c" %}main.c{% endfile %}
{% file src="string_test.c" %}string_test.c{% endfile %}
