#include <stdlib.h>
#include <stdio.h>
#include "stack.h"


/**@brief Initialisiert einen neuen intstack.
 * @param self  der zu initialisierende intstack
 * @return 0, falls keine Fehler bei der Initialisierung aufgetreten sind,
 *      != 0, ansonsten
 */
extern int stackInit(intstack_t* self)
{
    
    if(self == NULL)
    {
        fprintf(stderr, "Error!: %d, Memory creation failed.\n", MEMORY_EMPTY);
        return MEMORY_EMPTY;
    }
    
    self->top = NULL;
    
    return 0;
}



/**@brief Legt einen Wert auf den intstack.
 * @param self  der intstack
 * @param i     der Wert
 */
extern void stackPush(intstack_t* self, int i)
{
    if(self == NULL)
    {   fprintf(stderr, "Error!: %d, Memory creation failed.\n", MEMORY_NOT_FOUND);
        exit(MEMORY_NOT_FOUND);
    }
    
    input_stack *new_node = (input_stack*)malloc(sizeof(input_stack));
    if(new_node == NULL)
    {   fprintf(stderr, "Error!: %d, Memory creation failed.\n", MEMORY_NOT_FOUND);
        exit(MEMORY_NOT_FOUND);
    }
    new_node->value = i;
    new_node->next = self->top;
    self->top = new_node;
}

/**@brief Gibt das oberste Element des Stacks zurück.
 * @param self  der intstack
 * @return das oberste Element von \p self
 */
extern int stackTop(const intstack_t* self)
{
    if(self == NULL)
    {
        fprintf(stderr, "Error!: %d, No memory found.\n", MEMORY_NOT_FOUND);
        exit(MEMORY_NOT_FOUND);
    }
    return (self->top)->value;
}


/**@brief Entfernt und liefert das oberste Element des Stacks.
 * @param self  der intstack
 * @return das oberste Element von \p self
 */
extern int stackPop(intstack_t* self)
{
    if(self->top == NULL || self == NULL)
    {
        fprintf(stderr, "Error!: %d, No memory found.\n", MEMORY_NOT_FOUND);
        exit(MEMORY_NOT_FOUND);
    }
    
    input_stack *new_node = (input_stack*)malloc(sizeof(input_stack));
    if(new_node == NULL)
    {
        fprintf(stderr, "Error!: %d, No memory found.\n", MEMORY_NOT_FOUND);
        exit(MEMORY_NOT_FOUND);
    }
    int output = (self->top)->value;
    new_node = self->top->next;
    free(self->top);
    self->top = new_node;
    
    return output;
}


/**@brief Gibt zurück, ob der intstack leer ist.
 * @param self  der intstack
 * @return 0, falls nicht leer,
 != 0, falls leer
 */
extern int stackIsEmpty(const intstack_t* self)
{
    if(self == NULL)
    {
        fprintf(stderr, "Error!: %d, Memory is empty.\n", MEMORY_NOT_FOUND);
        exit(MEMORY_NOT_FOUND);
    }
    
    if(self->top == NULL)
    {
        return MEMORY_EMPTY;
    }
    
    return 0;
}


/**@brief Gibt den intstack und alle assoziierten Strukturen frei.
 * @param self  der freizugebende intstack
 */
extern void stackRelease(intstack_t* self)
{
    if(self == NULL)
    {
        fprintf(stderr, "Error!: %d, Memory is empty.\n", MEMORY_NOT_FOUND);
        exit(MEMORY_NOT_FOUND);
    }
    else{
        while(self->top != NULL){
           
            input_stack *new_node = (input_stack*)malloc(sizeof(input_stack));
            if(new_node == NULL)
            {
                fprintf(stderr, "Error!: %d, No memory found.\n", MEMORY_NOT_FOUND);
                exit(MEMORY_NOT_FOUND);
            }
            new_node = (self->top)->next;
            free(self->top);
            self->top = new_node;
            
        }
        
    }
}
