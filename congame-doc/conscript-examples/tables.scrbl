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
  @style{
    table {
      border-collapse: collapse;
      border: 2px solid rgb(200,200,200);
      letter-spacing: 1px;
      font-size: 0.8rem;
    }

    td, th {
      border: 1px solid rgb(190,190,190);
      padding: 10px 20px;
    }
    td {
      text-align: center;
    }
  }
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
  [basic-table --> table-with-header
               --> table-with-header-and-borders]
  [table-with-header-and-borders --> table-with-header-and-borders]
]
