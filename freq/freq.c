#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Declaring structs
*/
typedef struct line_t line_t;
typedef struct list_t list_t;
typedef struct link_t link_t;

struct line_t
{
    char text[256];
    int occurence_nbr;
};

/*
 * Defining functions
*/
list_t* new_list(void);
static link_t* new_link(void* data);
void insert_first(list_t* head, void* data);
int isPrime(int num);
int list_length(list_t* head);
void add_occurence(list_t* head, char* word, int increase_occurence);
void find_max_occurence(list_t* head);
int find_data(list_t* head, char* word);
void free_list(list_t* head);
void* xmalloc(size_t size);
void free_line_list(line_t* q);

int main(void)
{
    int line_nbr = 1;
    char word[256];

    // Create list
    list_t* word_list = new_list();

    while(scanf("%s", word) != EOF){
        //Check if the line is a prime number
        line_t* text_line = xmalloc(sizeof(*text_line));
        strncpy(text_line->text, word, sizeof(text_line->text) - 1); // Copy `word` into `text_line->text`
        text_line->text[sizeof(text_line->text) - 1] = '\0'; // Ensure null termination
        
        if(isPrime(line_nbr) == 1){
            //Delete occurens
            if(find_data(word_list, text_line->text) == 1){
                add_occurence(word_list, text_line->text, 0);
            } else {
                printf("trying to delete %s: not found\n", text_line->text);
            }
            free(text_line);
            text_line = NULL;

        } else {
            //Add occurence
            if(find_data(word_list, text_line->text) == 1){
                add_occurence(word_list, text_line->text, 1);
                free(text_line);
                text_line = NULL;
            } else {
                text_line->occurence_nbr = 1;
                printf("added %s\n", text_line->text);
                insert_first(word_list, text_line);
            }
        }
        line_nbr++;
    }

    find_max_occurence(word_list);
    free_list(word_list);
    word_list = NULL;
    return 0;
}

/*
 * Defining lists
*/
struct list_t
{
    link_t* first;
    link_t* last;
};

struct link_t
{
    link_t* next;
    void* data;
};

list_t* new_list(void)
{
    list_t* head;

    head = xmalloc(sizeof(list_t));
    head->first = NULL;
    head->last = NULL;
    return head;
}

static link_t* new_link(void* data)
{
    link_t* link;
    
    link = xmalloc(sizeof(link_t));
    link->next = NULL;
    link->data = data;
    return link;
}

void insert_first(list_t* head, void* data)
{
    link_t* link;
    link = new_link(data);
    if(head->last == NULL){
        head->last = link;
    } else {
        link->next = head->first;
    }
    head->first = link;
}

int find_data(list_t* head, char* word) {
    if (head->first == NULL) return 0;
    link_t* q = head->first;
    line_t* q_data = q->data;

    while (q != NULL) {
        if (strcmp(q_data->text, word) == 0) { // Corrected to check for equality
            return 1;
        }
        q = q->next;
        if (q != NULL) {
            q_data = q->data;
        }
    }
    return 0;
}

int list_length(list_t* head) 
{
    int size = 0;
    link_t* q = head->first;

    while (q != NULL) {
        size++;
        q = q->next;
    }

    return size;
}

void add_occurence(list_t* head, char* word, int increase_occurence)
{
    link_t* q = head->first;
    line_t* q_data = q->data;

    while(strcmp(q_data->text,word)){
        q = q->next;
        q_data = q->data;
    }
    
    if(increase_occurence == 1){
        if(q_data->occurence_nbr > 0){
            printf("counted %s\n", word);
        } else {
            printf("added %s\n", word);
        }
        q_data->occurence_nbr++;
    } else {
        if(q_data->occurence_nbr > 0){
            printf("trying to delete %s: deleted\n", word);
            q_data->occurence_nbr = 0;
        } else {
            printf("trying to delete %s: not found\n", word);
        }
    }
}

void free_list(list_t* head) {
    if (head == NULL) return;

    link_t* p = head->first;
    link_t* q;

    while (p != NULL) {
        q = p->next;
        // Free the link itself
        free(p->data);
        free(p);
        p = q;
    }

    // Now free the list structure itself
    free(head);
}

int isPrime(int num) 
{
    if (num <= 1) {
        return 0; // Numbers less than 2 are not prime
    }
    if (num <= 3) {
        return 1; // 2 and 3 are prime numbers
    }
    if (num % 2 == 0 || num % 3 == 0) {
        return 0; // Exclude multiples of 2 and 3
    }

    for (int i = 5; i * i <= num; i += 6) {
        if (num % i == 0 || num % (i + 2) == 0) {
            return 0; // Divisible by any i or i+2 implies not prime
        }
    }
    
    return 1; // Number is prime
}

void* xmalloc(size_t size)
{
   void* ptr = malloc(size);

   if (ptr == NULL) {
      fprintf(stderr, "out of memory\n");
      exit(1);
   }

   return ptr;
}

void find_max_occurence(list_t* head)
{
    link_t* q = head->first;
    line_t* highest_occ = q->data; 
    line_t *p;
    int nbr_occ = 0;

    while(q != NULL){
        p = q->data;
        if(p->occurence_nbr >= nbr_occ){
            nbr_occ = p->occurence_nbr;
            highest_occ = p;
        } else if(p->occurence_nbr == nbr_occ && *p->text < *highest_occ->text){
            nbr_occ = p->occurence_nbr;
            highest_occ = p;
        }
        q = q->next;
    }

    printf("result: %s %d\n", highest_occ->text, nbr_occ);
}
