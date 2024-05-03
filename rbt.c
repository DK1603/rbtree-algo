#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>

#define RB_DUP 1
#define RB_MIN 1

#define RED 0
#define BLACK 1

enum rbtraversal {
	PREORDER,
	INORDER,
	POSTORDER
};

typedef struct rbnode {
	struct rbnode *left;
	struct rbnode *right;
	struct rbnode *parent;
	char color;
	void *data;
} rbnode;

typedef struct {
	int (*compare)(const void *, const void *);
	void (*print)(void *);
	void (*destroy)(void *);

	rbnode root;
	rbnode nil;

	#ifdef RB_MIN
	rbnode *min;
	#endif
} rbtree;

#define RB_ROOT(rbt) (&(rbt)->root)
#define RB_NIL(rbt) (&(rbt)->nil)
#define RB_FIRST(rbt) ((rbt)->root.left)
#define RB_MINIMAL(rbt) ((rbt)->min)

#define RB_ISEMPTY(rbt) ((rbt)->root.left == &(rbt)->nil && (rbt)->root.right == &(rbt)->nil)
#define RB_APPLY(rbt, f, c, o) rbapply_node((rbt), (rbt)->root.left, (f), (c), (o))

rbtree *rb_create(int (*compare_func)(const void *, const void *), void (*destroy_func)(void *));
void rb_destroy(rbtree *rbt);

rbnode *rb_find(rbtree *rbt, void *data);
rbnode *rb_successor(rbtree *rbt, rbnode *node);

int rb_apply_node(rbtree *rbt, rbnode *node, int (*func)(void *, void *), void *cookie, enum rbtraversal order);
void rb_print(rbtree *rbt, void (*print_func)(void *));

rbnode *rb_insert(rbtree *rbt, void *data);
void *rb_delete(rbtree *rbt, rbnode *node, int keep);

int rb_check_order(rbtree *rbt, void *min, void *max);
int rb_check_black_height(rbtree *rbt);




static void insert_repair(rbtree *rbt, rbnode *current);
static void delete_repair(rbtree *rbt, rbnode *current);
static void rotate_left(rbtree *, rbnode *);
static void rotate_right(rbtree *, rbnode *);
static int check_order(rbtree *rbt, rbnode *n, void *min, void *max);
static int check_black_height(rbtree *rbt, rbnode *node);
static void print(rbtree *rbt, rbnode *node, void (*print_func)(void *), int depth, char *label);
static void destroy(rbtree *rbt, rbnode *node);

int cnt_left_delete = 0, cnt_right_delete = 0;
int cnt_left_insert = 0, cnt_right_insert = 0;

// construction
rbtree *rb_create(int (*compare)(const void *, const void *), void (*destroy)(void *))
{
	rbtree *rbt;

	rbt = (rbtree *) malloc(sizeof(rbtree));
	if (rbt == NULL)
		return NULL; 

	rbt->compare = compare;
	rbt->destroy = destroy;

	
	rbt->nil.left = rbt->nil.right = rbt->nil.parent = RB_NIL(rbt);
	rbt->nil.color = BLACK;
	rbt->nil.data = NULL;

	
	rbt->root.left = rbt->root.right = rbt->root.parent = RB_NIL(rbt);
	rbt->root.color = BLACK;
	rbt->root.data = NULL;

	#ifdef RB_MIN
	rbt->min = NULL;
	#endif
	
	return rbt;
}

/*
 * destruction
 */
void rb_destroy(rbtree *rbt)
{
	destroy(rbt, RB_FIRST(rbt));
	free(rbt);
}


rbnode *rb_find(rbtree *rbt, void *data)
{
	rbnode *p;

	p = RB_FIRST(rbt);

	while (p != RB_NIL(rbt)) {
		int cmp;
		cmp = rbt->compare(data, p->data);
		if (cmp == 0)
			return p; 
		p = cmp < 0 ? p->left : p->right;
	}

	return NULL; 
}


rbnode *rb_successor(rbtree *rbt, rbnode *node)
{
	rbnode *p;

	p = node->right;

	if (p != RB_NIL(rbt)) {
		
		for ( ; p->left != RB_NIL(rbt); p = p->left) ;
	} else {
		
		for (p = node->parent; node == p->right; node = p, p = p->parent) ;

		if (p == RB_ROOT(rbt))
			p = NULL; 
	}

	return p;
}


int rb_apply(rbtree *rbt, rbnode *node, int (*func)(void *, void *), void *cookie, enum rbtraversal order)
{
	int err;

	if (node != RB_NIL(rbt)) {
		if (order == PREORDER && (err = func(node->data, cookie)) != 0) // preorder
			return err;
		if ((err = rb_apply(rbt, node->left, func, cookie, order)) != 0) // left
			return err;
		if (order == INORDER && (err = func(node->data, cookie)) != 0) // inorder
			return err;
		if ((err = rb_apply(rbt, node->right, func, cookie, order)) != 0) // right
			return err;
		if (order == POSTORDER && (err = func(node->data, cookie)) != 0) // postorder
			return err;
	}

	return 0;
}

/*
 * rotate left about x
 */
void rotate_left(rbtree *rbt, rbnode *x)
{
	rbnode *y;

	y = x->right; 

	
	x->right = y->left;
	if (x->right != RB_NIL(rbt))
		x->right->parent = x;

	
	y->parent = x->parent;
	if (x == x->parent->left)
		x->parent->left = y;
	else
		x->parent->right = y;

	
	y->left = x;
	x->parent = y;
}

/*
 * rotate right about x
 */
void rotate_right(rbtree *rbt, rbnode *x)
{
	rbnode *y;

	y = x->left; 

	
	x->left = y->right;
	if (x->left != RB_NIL(rbt))
		x->left->parent = x;

	
	y->parent = x->parent;
	if (x == x->parent->left)
		x->parent->left = y;
	else
		x->parent->right = y;

	// assemble tree x and tree y 
	y->right = x;
	x->parent = y;
}



rbnode *rb_insert(rbtree *rbt, void *data)
{
	rbnode *current, *parent;
	rbnode *new_node;


	current = RB_FIRST(rbt);
	parent = RB_ROOT(rbt);

	while (current != RB_NIL(rbt)) {
		int cmp;
		cmp = rbt->compare(data, current->data);

		#ifndef RB_DUP
		if (cmp == 0) {
			rbt->destroy(current->data);
			current->data = data;
			return current; 
		}
		#endif

		parent = current;
		current = cmp < 0 ? current->left : current->right;
	}

	

	current = new_node = (rbnode *) malloc(sizeof(rbnode));
	if (current == NULL)
		return NULL;

	current->left = current->right = RB_NIL(rbt);
	current->parent = parent;
	current->color = RED;
	current->data = data;
	
	if (parent == RB_ROOT(rbt) || rbt->compare(data, parent->data) < 0)
		parent->left = current;
	else
		parent->right = current;

	#ifdef RB_MIN
	if (rbt->min == NULL || rbt->compare(current->data, rbt->min->data) < 0)
		rbt->min = current;
	#endif
	
	if (current->parent->color == RED) {
		insert_repair(rbt, current);
	} else {
	
	}
	RB_FIRST(rbt)->color = BLACK;
	
	return new_node;
}


void insert_repair(rbtree *rbt, rbnode *current)
{
	rbnode *uncle;

	do {
		/* current node is RED and parent node is RED */

		if (current->parent == current->parent->parent->left) {
			uncle = current->parent->parent->right;
			if (uncle->color == RED) {
				
				current->parent->color = BLACK;
				uncle->color = BLACK;

				
				current = current->parent->parent; 
				current->color = RED;
			} else {
				
				if (current == current->parent->right) {
					current = current->parent;
					rotate_left(rbt, current);
                    cnt_left_insert++;
				}

				
				current->parent->color = BLACK; 
				current->parent->parent->color = RED;
				rotate_right(rbt, current->parent->parent);
                cnt_right_insert++;
			}
		} else {
			uncle = current->parent->parent->left;
			if (uncle->color == RED) {
				
				current->parent->color = BLACK;
				uncle->color = BLACK;

				
				current = current->parent->parent; 
				current->color = RED;
			} else {
				
				if (current == current->parent->left) {
					current = current->parent;
					rotate_right(rbt, current);
                    cnt_right_insert++;
				}

				
				current->parent->color = BLACK; 
				current->parent->parent->color = RED;
				rotate_left(rbt, current->parent->parent);
                cnt_left_insert++;
			}
		}
	} while (current->parent->color == RED);
}

// delete node
void *rb_delete(rbtree *rbt, rbnode *node, int keep)
{
	rbnode *target, *child;
	void *data;
	
	data = node->data;

	
	if (node->left == RB_NIL(rbt) || node->right == RB_NIL(rbt)) {
		target = node;

		#ifdef RB_MIN
		if (rbt->min == target)
			rbt->min = rb_successor(rbt, target); 
		#endif
	} else {
		target = rb_successor(rbt, node); 

		node->data = target->data; 
		#ifdef RB_MIN
		
		#endif
	}

	child = (target->left == RB_NIL(rbt)) ? target->right : target->left; // child may be NIL 

	if (target->color == BLACK) {
		if (child->color == RED) {
			
			child->color = BLACK;
		} else if (target == RB_FIRST(rbt)) {
			
		} else {
			
			delete_repair(rbt, target);
		}
	} else {
		
	}

	if (child != RB_NIL(rbt))
		child->parent = target->parent;

	if (target == target->parent->left)
		target->parent->left = child;
	else
		target->parent->right = child;


	free(target);
	
	/* keep or discard data */
	if (keep == 0) {
		rbt->destroy(data);
		data = NULL;
	}

	return data;
}

/*
 * rebalance after deletion
 */
void delete_repair(rbtree *rbt, rbnode *current)
{
	rbnode *sibling;
	do {
		if (current == current->parent->left) {
			sibling = current->parent->right;

			if (sibling->color == RED) {
				
				sibling->color = BLACK;
				current->parent->color = RED;
				rotate_left(rbt, current->parent);
                cnt_left_delete++;
				sibling = current->parent->right;
			}

	
			if (sibling->right->color == BLACK && sibling->left->color == BLACK) {
				
				sibling->color = RED;
				if (current->parent->color == RED) { 
					current->parent->color = BLACK;
					break; 
				} else { 
					current = current->parent; 
				}
			} else {
				
				if (sibling->right->color == BLACK) {
					sibling->left->color = BLACK;
					sibling->color = RED;
					rotate_right(rbt, sibling);
                    cnt_right_delete++;
					sibling = current->parent->right;
				}

				
				sibling->color = current->parent->color;
				current->parent->color = BLACK;
				sibling->right->color = BLACK;
				rotate_left(rbt, current->parent);
                cnt_left_delete++;
				break; /* goto break */
			}
		} else {
			sibling = current->parent->left;

			if (sibling->color == RED) {
				sibling->color = BLACK;
				current->parent->color = RED;
				rotate_right(rbt, current->parent);
                cnt_right_delete++;
				sibling = current->parent->left;
			}

			

			if (sibling->right->color == BLACK && sibling->left->color == BLACK) {
				
				sibling->color = RED;
				if (current->parent->color == RED) { 
					current->parent->color = BLACK;
					break;
				} else { 
					current = current->parent; 
				}
			} else {
				
				if (sibling->left->color == BLACK) {
					sibling->right->color = BLACK;
					sibling->color = RED;
					rotate_left(rbt, sibling);
                    cnt_left_delete++;
					sibling = current->parent->left;
				}

				/* transfer by rotation and recoloring */
				sibling->color = current->parent->color;
				current->parent->color = BLACK;
				sibling->left->color = BLACK;
				rotate_right(rbt, current->parent);
                cnt_right_delete++;
				break; 
			}
		}
	} while (current != RB_FIRST(rbt));
}

/*
 * check order of tree
 */
int rb_check_order(rbtree *rbt, void *min, void *max)
{
	return check_order(rbt, RB_FIRST(rbt), min, max);
}

/*
 * check order recursively
 */
int check_order(rbtree *rbt, rbnode *n, void *min, void *max)
{
	if (n == RB_NIL(rbt))
		return 1;

	#ifdef RB_DUP
	if (rbt->compare(n->data, min) < 0 || rbt->compare(n->data, max) > 0)
	#else
	if (rbt->compare(n->data, min) <= 0 || rbt->compare(n->data, max) >= 0)
	#endif
		return 0;

	return check_order(rbt, n->left, min, n->data) && check_order(rbt, n->right, n->data, max);
}

/*
 * check black height of tree
 */
int rb_check_black_height(rbtree *rbt)
{
	if (RB_ROOT(rbt)->color == RED || RB_FIRST(rbt)->color == RED || RB_NIL(rbt)->color == RED)
		return 0;

	return check_black_height(rbt, RB_FIRST(rbt));
}

/*
 * check black height recursively
 */
int check_black_height(rbtree *rbt, rbnode *n)
{
	int lbh, rbh;

	if (n == RB_NIL(rbt))
		return 1;

	if (n->color == RED && (n->left->color == RED || n->right->color == RED || n->parent->color == RED))
		return 0;

	if ((lbh = check_black_height(rbt, n->left)) == 0)
		return 0;

	if ((rbh = check_black_height(rbt, n->right)) == 0)
		return 0;

	if (lbh != rbh)
		return 0;

	return lbh + (n->color == BLACK ? 1 : 0);
}

/*
 * print tree
 */
void rb_print(rbtree *rbt, void (*print_func)(void *))
{
	printf("\n>>>\n");
	print(rbt, RB_FIRST(rbt), print_func, 0, "T");
	//printf("\ncheck_black_height = %d\n", rb_check_black_height(rbt));
    printf("\n>>>\n");
}

/*
 * print node recursively
 */
void print(rbtree *rbt, rbnode *n, void (*print_func)(void *), int depth, char *label)
{
	if (n != RB_NIL(rbt)) {
		print(rbt, n->right, print_func, depth + 1, "R");
		printf("%*s", 8 * depth, "");
		if (label)
			printf("%s: ", label);
		print_func(n->data);
		printf(" (%s)\n", n->color == RED ? "r" : "b");
		print(rbt, n->left, print_func, depth + 1, "L");
	}
}

/*
 * destroy node recursively
 */
void destroy(rbtree *rbt, rbnode *n)
{
	if (n != RB_NIL(rbt)) {
		destroy(rbt, n->left);
		destroy(rbt, n->right);
		rbt->destroy(n->data);
		free(n);
	}
}

// rb_data.h 

typedef struct {
	int key;
} mydata;

mydata *makedata(int key);
int compare_func(const void *d1, const void *d2);
void destroy_func(void *d);
void print_func(void *d);
void print_char_func(void *d);


mydata *makedata(int key)
{
	mydata *p;

	p = (mydata *) malloc(sizeof(mydata));
	if (p != NULL)
		p->key = key;

	return p;
}

int compare_func(const void *d1, const void *d2)
{
	mydata *p1, *p2;
	
	assert(d1 != NULL);
	assert(d2 != NULL);
	
	p1 = (mydata *) d1;
	p2 = (mydata *) d2;
	if (p1->key == p2->key)
		return 0;
	else if (p1->key > p2->key)
		return 1;
	else
		return -1;
}

void destroy_func(void *d)
{
	mydata *p;
	
	assert(d != NULL);
	
	p = (mydata *) d;
	free(p);
}

void print_func(void *d)
{
	mydata *p;
	
	assert(d != NULL);
	
	p = (mydata *) d;
	printf("%d", p->key);
}

void print_char_func(void *d)
{
	mydata *p;
	
	assert(d != NULL);
	
	p = (mydata *) d;
	printf("%c", p->key & 127);
}


int main() {
    srand(time(NULL));  

    // Create a red black tree
    rbtree *rbt = rb_create(compare_func, destroy_func);

    // Insert 10 random unique integers into the tree
    printf("\n -------------------------------------------[Inserting random unique integers]-------------------------------------------\n");
    for (int i = 0; i < 10; i++) {
        int key = rand() % 20;  // Random key between 0 and 19
        mydata *data = makedata(key);
        if (!rb_find(rbt, data)) {  // Ensure no duplicate entries
            rb_insert(rbt, data);
            printf("--------> Inserted %d: ", key);
            rb_print(rbt, print_func);
            printf("\n");
            printf("Left rotations count(insertion): %d\n",cnt_left_insert);
            printf("Right rotations count(insertion): %d\n",cnt_right_insert);
            printf("Left rotations count(deletion): %d\n",cnt_left_delete);
            printf("Right rotations count(deletion): %d\n",cnt_right_delete);
            puts("");
        } else {
            free(data);  // Free data 
        }
    }

    // Sequential specific insertions
    int keys[] = {7, 12, 2, 18, 16};
    printf("\n-------------------------------------------[Sequential specific insertions]-------------------------------------------\n");
    for (int i = 0; i < sizeof(keys) / sizeof(keys[0]); i++) {
        mydata *data = makedata(keys[i]);
        if (!rb_find(rbt, data)) {
            rb_insert(rbt, data);
            printf("After inserting %d: ", keys[i]);
            rb_print(rbt, print_func);
            printf("\n");
            printf("Left rotations count(insertion): %d\n",cnt_left_insert);
            printf("Right rotations count(insertion): %d\n",cnt_right_insert);
            printf("Left rotations count(deletion): %d\n",cnt_left_delete);
            printf("Right rotations count(deletion): %d\n",cnt_right_delete);
            puts("");
        } else {
            free(data);  // Free data if not inserted
        }
    }
    
     // Keys to be deleted as specified, including placeholder for root-key
    int delete_keys[] = {18, -1, 2, -1, -1}; // -1 will indicate deletion of root-key
    int isRoot = 0;

    printf("\n-------------------------------------------[Deleting specified keys]-------------------------------------------\n");
    for (int i = 0; i < sizeof(delete_keys) / sizeof(delete_keys[0]); i++) {
        int key_to_delete;
        if (delete_keys[i] == -1) {  // Check if the instruction is to delete the root-key
            if (RB_ROOT(rbt) != RB_NIL(rbt)) {
                key_to_delete = *((int*)RB_FIRST(rbt)->data);
                isRoot = 1;
            } else {
                printf("Tree is empty, no root to delete.\n");
                continue;
            }
        } else {
            key_to_delete = delete_keys[i];
            isRoot =0;
        }

        mydata dummy = {key_to_delete};
        rbnode *node = rb_find(rbt, &dummy);
        if (node) {
            rb_delete(rbt, node, 1);  // '1' indicates freeing the node's data
            //printf("--------> After deleting %d:\n", key_to_delete);
            if(isRoot == 1 && key_to_delete){
                printf("--------> After deleting root %d:\n", key_to_delete);

            }
            else {
                printf("--------> After deleting %d:\n", key_to_delete);
            }
            rb_print(rbt, print_func);
            printf("Left rotations count(insertion): %d\n", cnt_left_insert);
            printf("Right rotations count(insertion): %d\n", cnt_right_insert);
            printf("Left rotations count(deletion): %d\n", cnt_left_delete);
            printf("Right rotations count(deletion): %d\n", cnt_right_delete);
            puts("");
        } else {
            printf("Key %d not found in the tree.\n", key_to_delete);
        }
    }

    // Clean up the tree
    rb_destroy(rbt);

    return 0;
}

