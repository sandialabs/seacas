#include <stdlib.h>
#include <stdio.h>
#include "matio.h"

int
main(int argc,char **argv)
{
    mat_t    *matfp;
    matvar_t *cell_array, *cell_element;
    size_t    dims[2] = {10,1};
    double    x[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9,10},
              y[10] = {11,12,13,14,15,16,17,18,19,20};
    struct mat_complex_split_t z = {x,y};

    matfp = Mat_CreateVer("test.mat",NULL,MAT_FT_DEFAULT);
    if ( NULL == matfp ) {
        fprintf(stderr,"Error creating MAT file \"test.mat\"\n");
        return EXIT_FAILURE;
    }

    dims[0] = 3;
    dims[1] = 1;
    cell_array = Mat_VarCreate("a",MAT_C_CELL,MAT_T_CELL,2,dims,NULL,0);
    if ( NULL == cell_array ) {
        fprintf(stderr,"Error creating variable for 'a'\n");
        Mat_Close(matfp);
        return EXIT_FAILURE;
    }

    dims[0] = 10;
    dims[1] = 1;
    cell_element = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,dims,x,0);
    if ( NULL == cell_element ) {
        fprintf(stderr,"Error creating cell element variable\n");
        Mat_VarFree(cell_array);
        Mat_Close(matfp);
        return EXIT_FAILURE;
    }
    Mat_VarSetCell(cell_array,0,cell_element);

    cell_element = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,dims,y,0);
    if ( NULL == cell_element ) {
        fprintf(stderr,"Error creating cell element variable\n");
        Mat_VarFree(cell_array);
        Mat_Close(matfp);
        return EXIT_FAILURE;
    }
    Mat_VarSetCell(cell_array,1,cell_element);

    cell_element = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,dims,&z,
                      MAT_F_COMPLEX);
    if ( NULL == cell_element ) {
        fprintf(stderr,"Error creating cell element variable\n");
        Mat_VarFree(cell_array);
        Mat_Close(matfp);
        return EXIT_FAILURE;
    }
    Mat_VarSetCell(cell_array,2,cell_element);

    Mat_VarWrite(matfp,cell_array,MAT_COMPRESSION_NONE);
    Mat_VarFree(cell_array);

    Mat_Close(matfp);

    return EXIT_SUCCESS;
}
