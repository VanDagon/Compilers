//
//  syntree.c
//  syntree
//
//  Created by  Taeshin 578277, Junhyuk KO 531806 on 06.05.19.
//  Copyright © 2019 TS. All rights reserved.
//

#include <stdlib.h>
#include <stdio.h>
#include "syntree.h"

extern int syntreeInit(syntree_t* self)
{
    self->current = 0;
    self->top = 1;
    self->nodes = malloc(sizeof(Node) * (self->top));
    if (self->nodes == NULL) return -100;
    
    return 0;
}


void syntreeRelease(syntree_t* self) {
    free(self->nodes);
}

syntree_nid syntreeNodeNumber(syntree_t* self, int number) {
    if (memoryExtension(self)) return -1;
    self->nodes[self->current].value = number;
    self->nodes[self->current].next = -1;
    self->nodes[self->current].successor_node = -1;
    return self->current++;
}

syntree_nid syntreeNodeTag(syntree_t* self, syntree_nid id) {
    if (memoryExtension(self)) return -1;
    
    self->nodes[self->current].value = -1;
    self->nodes[self->current].next = -1;
    self->nodes[self->current].successor_node = id;
    self->nodes[id].pre_node = self->current;
    return self->current++;
}


syntree_nid syntreeNodePair(syntree_t* self, syntree_nid id1, syntree_nid id2) {
    if (memoryExtension(self)) return (syntree_nid) -1;
    
    self->nodes[self->current].value = -1;
    self->nodes[self->current].next = -1;
    self->nodes[self->current].successor_node = id1;
    self->nodes[id1].next = self->nodes[id2].pre_node;
    self->nodes[id1].pre_node = self->current;
    return self->current++;
}


syntree_nid syntreeNodeAppend(syntree_t* self, syntree_nid list, syntree_nid elem) {
    syntree_nid successorId = self->nodes[list].successor_node;
    while (self->nodes[successorId].next != -1)
        successorId = self->nodes[successorId].next;
    self->nodes[successorId].next = elem;
    self->nodes[elem].pre_node = list;
    return elem;
}


syntree_nid syntreeNodePrepend(syntree_t* self, syntree_nid elem, syntree_nid list) {
    syntree_nid preId = self->nodes[list].pre_node;
    self->nodes[elem].next = self->nodes[preId].successor_node;
    self->nodes[elem].pre_node = preId;
    self->nodes[preId].successor_node = elem;
    return elem;
}

void syntreePrint(const syntree_t* self, syntree_nid root) {
    syntreePrintNode(self, root);
    printf("\n");
}


void syntreePrintNode(const syntree_t *self, syntree_nid nodeId) {
    if (self->nodes[nodeId].successor_node == -1) {
        printf("(%d)", self->nodes[nodeId].value);
    }
    else
    {
        printf("{");
        
        syntreePrintNode(self, self->nodes[nodeId].successor_node);
        printf("}");
    }
    
    if (self->nodes[nodeId].next != -1) {
        syntreePrintNode(self, self->nodes[nodeId].next);
    }
}


int memoryExtension(syntree_t *self) {
    if (self->current == self->top) {
        self->top = 2* self->top;
        self->nodes = realloc(self->nodes, sizeof(Node)*(self->top));
        if (self->nodes == NULL)
            return -200;
    }
    return 0;
}
