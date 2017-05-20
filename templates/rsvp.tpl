<bind tag="class">rsvp</bind>
<apply template="_base">

    <h3>
        RSVP Code <span><k/></span>
    </h3>

    <not-confirmed>
        <p>
            Is this not you? Please email us at <a href="mailto:contact@amydanielwedding.com">contact@amydanielwedding.com</a> and we'll get you the right code.
    </p>
    </not-confirmed>

    <confirmed>
            <p>You're all set! If anything changes, you can still make changes below, but be sure to click the "Update" button afterwards.</p>
    </confirmed>

    <dfForm class="styled ${confirmed-class}" ref="rsvp">

        <h4>Attendees</h4>

        <people>
            <div class="check person">
                    <div class="checkbox">
                        <locked>
                            <dfInputCheckbox ref="person-${id}-include"/>
                            <dfLabel ref="person-${id}-include">
                                <name/>
                                <em>will attend</em>
                        </dfLabel>
                </locked>
                <not-locked>
                    <dfInputText ref="person-${id}-name"/>
                    <dfInputCheckbox ref="person-${id}-include"/>
                    <dfLabel ref="person-${id}-include"> 
                        <em>will attend</em>
                    </dfLabel>
                </not-locked>
                    </div>
            </div>
        </people>

        <h4>Lodging</h4>


        <div class="check">
            <dfLabel ref="lodging">
                <em>We prefer to stay in</em>
                <dfInputSelect ref="lodging" />
            </dfLabel>
        </div>


        <div class="check">
            <div class="checkbox night">
                <dfInputCheckbox ref="friday"/>
                <dfLabel ref="friday">

                    <em>On</em> Friday, September 15th, <em>we will stay the night</em>


                </dfLabel>
            </div>
        </div>

        <div class="check">
            <div class="checkbox night">
                <dfInputCheckbox ref="saturday" />
                <dfLabel ref="saturday">

                    <em>On</em> Saturday, September 16th, <em>we will stay the night</em>

            </dfLabel>
            </div>
        </div>

        <p>Based on your response, we will follow up with you about how to book your accomodations.</p>

        <h4>Food</h4>

                <dfLabel ref="food">
                    <p>Please describe any food allergies / restrictions, and
                    the number of people in your party with them.</p>
                    <dfInputTextArea ref="food"/>
                </dfLabel>

                <not-confirmed>
                    <dfInputSubmit value="Confirm"/>
                </not-confirmed>
                <confirmed>
                    <dfInputSubmit value="Update"/>
                </confirmed>
    </dfForm>

    <script type="text/javascript">
     function update_person_message (elem) {
         window.elem = elem;
         if (elem.parent().find("input")[0].checked) {
             elem.find("em").text("will attend");
         } else {
             elem.find("em").text("will not attend");
         }
     }
     $(".person label").on("click", function () {
         var x = $(this);
         window.setTimeout(function () {
             update_person_message(x);
         }, 0);
     });
     $(".person label").each(function (_, elem) {
         update_person_message($(elem));
     });


     function update_night_message (elem) {
         window.elem = elem;
         if (elem.parent().find("input")[0].checked) {
             elem.find("em").last().text("we will stay the night");
         } else {
             elem.find("em").last().text("we will not stay the night");
         }
     }
     $(".night label").on("click", function () {
         var x = $(this);
         window.setTimeout(function () {
             update_night_message(x);
         }, 0);
     });
     $(".night label").each(function (_, elem) {
         update_night_message($(elem));
     });

    </script>

</apply>
