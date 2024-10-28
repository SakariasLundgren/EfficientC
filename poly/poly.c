#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "error.h"
#include "poly.h"

void bp(void){}

struct poly_t{
    int* exponent; // The value of the "x" exponent
    int* constant; // The value of the constant
    int length; // How many terms the polynom has. TODO: Might not be needed
};

void* xmalloc(size_t size)
{
   void* ptr = malloc(size);
   if (ptr == NULL) {
      fprintf(stderr, "out of memory\n");
      exit(1);
   }
   return ptr;
}

int count_occurences(const char* text, const char* text_pattern)
{
    int count = 0;
    const char *temp = text;

    // Loop over the text to find pattern
    while((temp = strstr(temp, text_pattern)) != NULL){
        count++;
        temp += strlen(text_pattern); //Move past found pattern
    }
    return count;
}

// Description: Takes a string and creates poly_t
poly_t* new_poly_from_string(const char* str_polynom)
{
    int i = 0;
    int poly_length = 0;
    int sign_flag = 0;
    const char* temp_str = str_polynom; 

    // Find number of + or - there are to create the correct length lists
    poly_length = count_occurences(str_polynom, "-") + count_occurences(str_polynom, "+") + 1;
    //printf("count: %d\n", poly_length);

    poly_t* p = xmalloc(sizeof(poly_t));
    p->exponent = calloc(poly_length,sizeof(int));   
    p->constant = calloc(poly_length, sizeof(int));
    p->length = poly_length;
    //printf("length: %d\n", p->length);

    while(*temp_str != '\0'){
        if(*temp_str >= '0' && *temp_str <= '9'){
            //Add to constant list.
            while(*temp_str >= '0' && *temp_str <= '9'){
                p->constant[i] = 10 * p->constant[i] + (*temp_str - '0');
                temp_str++;
            }

            if(sign_flag){
                p->constant[i] = p->constant[i] * -1;
                sign_flag = 0;
            }

            temp_str--;
        } else if(*temp_str == 'x') {
            // Save exponent
            temp_str++;
            if(*temp_str == '^'){
                temp_str++;
                while(*temp_str >= '0' && *temp_str <= '9'){
                    p->exponent[i] = 10 * p->exponent[i] + (*temp_str - '0');
                    temp_str++;
                }
                temp_str--;
            } else {
                p->exponent[i] = 1;
                temp_str--;
            }
        } else if(*temp_str == '-') {
            sign_flag = 1;
            i++;
        } else if(*temp_str == '+'){
            sign_flag = 0;
            i++;
        }
        temp_str++; // Next letter
    }

    for(i = 0; i < poly_length; i++)
        if(p->constant[i] == 0)
            p->constant[i] = 1;
    return p;
}

void free_poly(poly_t* p)
{
    free(p->exponent);
    free(p->constant);
    free(p);
}

poly_t* mul(poly_t* a, poly_t* b)
{
    //Fix that they are displayed correctly
    int i, j, k, count, exponent, skip_count;
    int combined_length = a->length + b->length;
    poly_t* answer = xmalloc(sizeof(poly_t));
    answer->constant = calloc(combined_length, sizeof(int));
    answer->exponent = calloc(combined_length, sizeof(int));

    count = 0;
    skip_count = 0;

    for(i = 0; i < a->length; i++){
        for(j = 0; j < b->length; j++){
            exponent = a->exponent[i] + b->exponent[j];
            skip_count = 0;

            // If the exponent is already in the list, add the constants together
            for(k = 0; k < count + 1; k++){
                if(answer->exponent[k] == exponent){
                    answer->constant[k] = answer->constant[k] + (a->constant[i] * b->constant[j]);
                    skip_count = 1;
                    break;
                }
            }

            if(skip_count != 1){
                answer->constant[count] = a->constant[i] * b->constant[j];
                answer->exponent[count] = a->exponent[i] + b->exponent[j];  
                //printf("%dx^%d \n", answer->constant[count], answer->exponent[count]);
                count++;
            }
        }
    }
    answer->length = count + 1;

    return answer;
}

void print_poly(poly_t* p)
{
    for (int i = 0; i < p->length; i++) {
        //Print constants
        if(p->constant[i] > 1){
            printf("%d", p->constant[i]);
        }else if(p->constant[i] < 0){
            printf("%d", p->constant[i] * -1);
        }else if (p->constant[i] == 1 && p->exponent[i] == 0){
            printf("1");
        }

        if(p->exponent[i] > 1){
            printf("x^%d", p->exponent[i]);
        } else if (p->exponent[i] == 1){
            printf("x");
        }
        
        if(i != p->length - 1) {
            if(p->constant[i + 1] > 0){
                printf(" + ");
            } else {
                printf(" - ");
            }
        } 
    }
    printf("\n");
}
