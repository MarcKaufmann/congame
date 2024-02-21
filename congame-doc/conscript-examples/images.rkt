#lang conscript

(provide
 image-tutorial)

(defstep (instructions)
  @md{
    # How to use Images in your Studies

    There are two ways of adding images to your studies:

    1. By adding them to the `/static/img` folder which is bundled with the server executable.
    2. Uploading them for each study instance by creating a study-specific admin page where you - the owner of the study - can upload a file that you can reuse elsewhere in the same study instance

    Here we will go through the steps needed for the first approach.

    @button{Continue}})

(defstep (add-image-to-server)
  @md{
    # Add Image to Server

    Suppose you have an image called `play.png` that you want to add to the study. Then the first step is to add the image to your server's `/static/img` folder.

    If you do not have access to it, send the image to a person who can add the image to the folder. Make sure they tell you exactly where the image is stored, for example under `/static/img/play.png`.

    @button{Next Step}})

(defstep (add-image-to-page-with-md)
  @md{
    # Add Image to a Page with `md`

    Suppose that you wanted to include an image on a page. Then you have to add the following code to the page when using markdown syntax (with `md`):

    ```
    ![The caption for your image.](/static/img/play.png)
    ```

    This would display as follows:

    ![The caption for your image.](/static/img/play.png)

    @button{Next Step}})

(defstep (add-image-to-page-with-html)
  @md{
    # Add Image to a Page with `html`

    While markdown syntax is might convenient, it does not allow adding an id or class to HTML elements, making it hard to style images. In that case, you may have to use `html`. To do so, include the following code inside of `html` or `html*`:

    ```
    @"@"img[#:alt "The Caption" #:src "/static/img/play.png"]
    ```

    To add the class 'highlight' to the image, just write

    ```
    @"@"img[#:alt "The Caption" #:src "/static/img/play.png" #:class "highlight"]
    ```

    You can use this inside of `md` by using `html*`:

    ```
    @"@"md{
      ... Some stuff ...

      The image:

      @"@"html*{
        @"@"img[#:alt "The Caption" #:src "/static/img/play.png" #:class "highlight"]
      }
    }
    ```

    @button{Next Step}})

(define my-classy-image
  @html*{
    @img[#:alt "The Caption" #:src "/static/img/play.png" #:class "highlight"]})

(defstep (reuse-image)
  @md{
    # How to reuse an image

    If you want to reuse the image again and again, then define the reusable HTML snippet first with `html*`:
    ```
    (define my-classy-image
      @"@"html*{
        @"@"img[#:alt "The Caption" #:src "/static/img/play.png" #:class "highlight"]
      })
    ```

    Then all you have to do to insert it is to write `@"@"my-classy-image` to include it in your page. Including it twice in a row requires simply:
    ```
    @"@"my-classy-image
    @"@"my-classy-image
    ```
    and looks like this:

    @my-classy-image
    @my-classy-image

    Beautiful, innit?

    @button{Next}})

(defstep (end)
  @md{
    # The End

    This is the end of the tutorial on using images in conscript. If you want to start from the beginning, click below.

    @button{Start Again}})

(defstudy image-tutorial
  [instructions --> add-image-to-server
                --> add-image-to-page-with-md
                --> add-image-to-page-with-html
                --> reuse-image
                --> end
                --> instructions])
