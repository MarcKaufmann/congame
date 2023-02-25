@step[basic-table]{
  @h1{A basic table without headers}

  @table{
    @tbody{
      @tr{
        @td{Item 1} @td{Property 1}}
      @tr{
        @td{Item 2} @td{Property 2}}
    }
  }

  @button{Next}
}

@step[table-with-header]{
  @h1{A table with headers}

  @table{
    @thead{
      @tr{
        @th{Item} @th{Property}}
    }
    @tbody{
      @tr{
        @td{Item 1} @td{Property 1}}
      @tr{
        @td{Item 2} @td{Property 2}}
    }
  }

  @button{Next}
}

@step[table-with-header-and-borders]{
  @h1{A table with headers and borders}

  @table{
    @thead{
      @tr{
        @th{Item} @th{Property}}
    }
    @tbody{
      @tr{
        @td{Item 1} @td{Property 1}}
      @tr{
        @td{Item 2} @td{Property 2}}
    }
  }

}

@study[
  show-tables
  #:transitions
  [basic-table --> table-with-header]
  [table-with-header --> table-with-header]
]
